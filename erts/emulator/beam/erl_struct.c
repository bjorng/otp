/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2024. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_struct.h"
#include "global.h"
#include "index.h"

#include "bif.h"

#define STRUCT_INITIAL_SIZE   4000
#define STRUCT_LIMIT          (512*1024)

#define STRUCT_HASH(module, name, arity)                                      \
    ((atom_val(module) * atom_val(name)) ^ (arity))

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

/* Note: the active table is never locked */
static IndexTable struct_tables[ERTS_NUM_CODE_IX];

static erts_atomic_t total_entries_bytes;

/* This lock protects the staging struct table from concurrent access
 * AND it protects the staging table from becoming active. */
erts_mtx_t struct_staging_lock;

struct struct_hash_entry
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    ErtsStructEntry *sp;
};

/* Helper struct that brings things together in one allocation
*/
struct struct_blob
{
    ErtsStructEntry str; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    struct struct_hash_entry entryv[ERTS_NUM_CODE_IX];
    /* Note that entryv is not indexed by "code_ix". */
};

/* Helper struct only used as template
*/
struct struct_templ
{
    struct struct_hash_entry entry;
    ErtsStructEntry str;
};

static struct struct_blob *entry_to_blob(struct struct_hash_entry *se)
{
    return ErtsContainerStruct(se->sp, struct struct_blob, str);
}

static HashValue
struct_hash(struct struct_hash_entry *se)
{
    ErtsStructEntry *str = se->sp;
    return STRUCT_HASH(str->module, str->name, str->arity);
}

static int
struct_cmp(struct struct_hash_entry* tmpl_e, struct struct_hash_entry* obj_e)
{
    const ErtsStructEntry *tmpl = tmpl_e->sp, *obj = obj_e->sp;

    return !(tmpl->module == obj->module &&
             tmpl->name == obj->name &&
             tmpl->arity == obj->arity);
}

static struct struct_hash_entry*
struct_alloc(struct struct_hash_entry* tmpl_e)
{
    struct struct_blob* blob;
    unsigned ix;

    if (tmpl_e->slot.index == -1) {
        /* Template, allocate blob */
        ErtsStructEntry *tmpl = tmpl_e->sp;
        ErtsStructEntry *obj;

        /* Force-align the address of struct entries so that they can safely be
         * placed in a small integer. */
        blob = (struct struct_blob*)
            erts_alloc_permanent_aligned(ERTS_ALC_T_STRUCT, sizeof(*blob),
                                         (1 << _TAG_IMMED1_SIZE));
        erts_atomic_add_nob(&total_entries_bytes, sizeof(*blob));
        obj = &blob->str;

        ASSERT(((UWord)obj) % (1 << _TAG_PRIMARY_SIZE) == 0);

        obj->module = tmpl->module;
        obj->name = tmpl->name;
        obj->arity = tmpl->arity;

        for (ix = 0; ix < ERTS_NUM_CODE_IX; ix++) {
            blob->entryv[ix].slot.index = -1;
            blob->entryv[ix].sp = &blob->str;

            obj->definitions[ix] = THE_NON_VALUE;
        }

        ix = 0;
    } else {
        /* Existing entry in another table, use free entry in blob */
        blob = entry_to_blob(tmpl_e);

        for (ix = 0; blob->entryv[ix].slot.index >= 0; ix++) {
            ASSERT(ix < ERTS_NUM_CODE_IX);
        }
    }

    return &blob->entryv[ix];
}

static void
struct_free(struct struct_hash_entry* obj)
{
    struct struct_blob* blob = entry_to_blob(obj);
    int i;

    obj->slot.index = -1;

    for (i=0; i < ERTS_NUM_CODE_IX; i++) {
        if (blob->entryv[i].slot.index >= 0) {
            return;
        }
    }

    erts_free(ERTS_ALC_T_EXPORT, blob);
    erts_atomic_add_nob(&total_entries_bytes, -sizeof(*blob));
}

void erts_struct_init_table(void)
{
    HashFunctions f;
    int i;

    erts_mtx_init(&struct_staging_lock, "struct_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    erts_atomic_init_nob(&total_entries_bytes, 0);

    f.hash = (H_FUN) struct_hash;
    f.cmp  = (HCMP_FUN) struct_cmp;
    f.alloc = (HALLOC_FUN) struct_alloc;
    f.free = (HFREE_FUN) struct_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
        erts_index_init(ERTS_ALC_T_STRUCT_TABLE, &struct_tables[i],
                        "struct_list", STRUCT_INITIAL_SIZE, STRUCT_LIMIT, f);
    }
}

static struct struct_hash_entry* init_template(struct struct_templ* templ,
                                          Eterm module, Eterm name, Uint arity)
{
    templ->entry.sp = &templ->str;
    templ->entry.slot.index = -1;
    templ->str.module = module;
    templ->str.name = name;
    templ->str.arity = arity;
    return &templ->entry;
}

/* Declared extern in header */
ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                   Eterm name,
                                   Uint arity,
                                   ErtsCodeIndex code_ix);

ErtsStructEntry *erts_struct_find_entry(Eterm module,
                                   Eterm name,
                                   Uint arity,
                                   ErtsCodeIndex code_ix)
{
    struct struct_templ templ;
    struct struct_hash_entry* ee;

    ASSERT(code_ix != erts_staging_code_ix());

    ee = hash_get(&struct_tables[code_ix].htable,
                  init_template(&templ, module, name, arity));

    if (ee) {
        return ee->sp;
    }

    return NULL;
}

ErtsStructEntry *erts_struct_put(Eterm module, Eterm name, Uint arity)
{
    ErtsCodeIndex code_ix = erts_staging_code_ix();
    struct struct_templ templ;
    struct struct_hash_entry* ee;

    ASSERT(is_atom(module));
    ASSERT(is_atom(name));

    erts_struct_staging_lock();

    ee = (struct struct_hash_entry*)
        index_put_entry(&struct_tables[code_ix],
                        init_template(&templ, module, name, arity));

    erts_struct_staging_unlock();

    return ee->sp;
}

ErtsStructEntry *erts_struct_get_or_make_stub(Eterm module,
                                              Eterm name,
                                              Uint arity)
{
    ErtsCodeIndex code_ix;
    ErtsStructEntry *sp;
    IF_DEBUG(int retrying = 0;)

    ASSERT(is_atom(module));
    ASSERT(is_atom(name));

    do {
        code_ix = erts_active_code_ix();
        sp = erts_struct_find_entry(module, name, arity, code_ix);

        if (sp == NULL) {
            /* The code is not loaded (yet). Put the struct in the staging
             * struct table, to avoid having to lock the active struct
             * table. */
            erts_struct_staging_lock();

            if (code_ix == erts_active_code_ix()) {
                struct struct_templ templ;
                struct struct_hash_entry* entry;

                IndexTable *tab = &struct_tables[erts_staging_code_ix()];

                init_template(&templ, module, name, arity);
                entry = (struct struct_hash_entry *)
                    index_put_entry(tab, &templ.entry);
                sp = entry->sp;

                ASSERT(sp);
            } else {
                /* race */
                ASSERT(!retrying);
                IF_DEBUG(retrying = 1);
            }

            erts_struct_staging_unlock();
        }
    } while (!sp);

    return sp;
}

IF_DEBUG(static ErtsCodeIndex debug_struct_load_ix = 0;)

void erts_struct_start_staging(void)
{
    ErtsCodeIndex dst_ix = erts_staging_code_ix();
    ErtsCodeIndex src_ix = erts_active_code_ix();
    IndexTable* dst = &struct_tables[dst_ix];
    IndexTable* src = &struct_tables[src_ix];
    int i;

    ASSERT(dst_ix != src_ix);
    ASSERT(debug_struct_load_ix == ~0);

    erts_struct_staging_lock();

    /* Insert all entries in src into dst table */
    for (i = 0; i < src->entries; i++) {
        struct struct_hash_entry* src_entry;
        ErtsStructEntry *sp;

        src_entry = (struct struct_hash_entry*) erts_index_lookup(src, i);
        sp = src_entry->sp;

        sp->definitions[dst_ix] = sp->definitions[src_ix];

#ifndef DEBUG
        index_put_entry(dst, src_entry);
#else /* DEBUG */
        {
            struct struct_hash_entry* dst_entry =
                (struct struct_hash_entry*)index_put_entry(dst, src_entry);
            ASSERT(entry_to_blob(src_entry) == entry_to_blob(dst_entry));
        }
#endif
    }

    erts_struct_staging_unlock();

    IF_DEBUG(debug_struct_load_ix = dst_ix);
}

void erts_struct_end_staging(int commit)
{
    ASSERT(debug_struct_load_ix == erts_staging_code_ix());
    IF_DEBUG(debug_struct_load_ix = ~0);
}

BIF_RETTYPE struct_prototype_define_3(BIF_ALIST_3) {
    /* Module, Name, {{Key, Default}, ...} */
    Eterm module, name, definition;
    Eterm *pairs;
    int arity;

    module = BIF_ARG_1;
    name = BIF_ARG_2;
    definition = BIF_ARG_3;

    /* For simplicity, the field definition is a tuple rather than a list or
     * map. We do not use the latter because a map wouldn't retain the field
     * order, which we haven't decided whether to keep. */
    if (!is_atom(module) ||
        !is_atom(name) ||
        !is_tuple(definition)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    pairs = tuple_val(BIF_ARG_3);
    arity = arityval(*pairs);

    if (arity <= 1 || arity > MAX_ARG) {
        BIF_ERROR(BIF_P, BADARG);
    }

    for (int i = 1; i <= arity; i++) {
        if (!is_tuple_arity(pairs[i], 2)) {
            BIF_ERROR(BIF_P, BADARG);
        } else {
            Eterm *pair = tuple_val(pairs[i]);

            /* Keys must be atoms, and default values must be immediates for
             * now. The latter restriction will be lifted once we move the
             * definition to the defining module's literal area. */
            if (!is_atom(pair[1]) || !is_immed(pair[2])) {
                BIF_ERROR(BIF_P, BADARG);
            }
        }
    }

    if (!erts_try_seize_code_load_permission(BIF_P)) {
        ERTS_BIF_YIELD3(BIF_TRAP_EXPORT(BIF_struct_prototype_define_3),
                        BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    erts_start_staging_code_ix(0);
    {
        const Uint sz = 2 + (2 * arity);
        ErtsStructDefinition *defp;
        ErtsStructEntry *entry;
        Eterm def;

        /* We don't have a module to tie ourselves to, allocate the struct
         * definition as a forever-leaking literal for now. */
        defp = erts_alloc(ERTS_ALC_T_LITERAL, sz * sizeof(Eterm));
        def = make_boxed((Eterm*)defp);
        erts_set_literal_tag(&def, (Eterm*)defp, sz);

        defp->thing_word = make_arityval(sz - 1);
        for (int i = 0; i < arity; i++) {
            Eterm *pair = tuple_val(pairs[1 + i]);
            defp->fields[i].key = pair[1];
            defp->fields[i].value = pair[2];
        }

        entry = erts_struct_put(BIF_ARG_1, BIF_ARG_2, arity);
        entry->definitions[erts_staging_code_ix()] = def;

        defp->entry = make_small((Uint)entry);
    }
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();

    erts_release_code_load_permission();

    BIF_RET(am_true);
}

BIF_RETTYPE struct_prototype_create_3(BIF_ALIST_3) {
    /* Module, Name, Arity */
    Eterm module, name, arity;
    ErtsStructEntry *entry;
    Uint code_ix;

    module = BIF_ARG_1;
    name = BIF_ARG_2;
    arity = BIF_ARG_3;

    if (!is_atom(module) ||
        !is_atom(name) ||
        !(is_small(arity) && unsigned_val(arity) <= MAX_ARG)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    code_ix = erts_active_code_ix();
    entry = erts_struct_find_entry(module,
                                   name,
                                   unsigned_val(arity),
                                   code_ix);

    if (entry != NULL) {
        Eterm def = entry->definitions[code_ix];

        if (def != THE_NON_VALUE) {
            ErtsStructDefinition *defp;
            int field_count;
            Eterm *hp;

            defp = (ErtsStructDefinition*)boxed_val(def);
            field_count = (arityval(defp->thing_word) - 1) / 2;
            hp = HAlloc(BIF_P, 2 + field_count);
            hp[0] = MAKE_STRUCT_HEADER(field_count);
            hp[1] = def;

            for (int i = 0; i < field_count; i++) {
                hp[2 + i] = defp->fields[i].value;
            }

            BIF_RET(make_struct(hp));
        }
    }

    /* No canonical struct definition, bail out. */
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE struct_prototype_update_3(BIF_ALIST_3) {
    /* Struct term, Key, Value */
    ErtsStructDefinition *defp;
    ErtsStructEntry *entry;
    int field_count;
    Uint code_ix;

    Eterm obj, *objp;
    Eterm key, value;
    Eterm *hp;

    obj = BIF_ARG_1;
    key = BIF_ARG_2;
    value = BIF_ARG_3;

    if (!is_struct(obj) ||
        !is_atom(key)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    objp = struct_val(obj);
    field_count = header_arity(objp[0]) - 1;
    defp = (ErtsStructDefinition*)boxed_val(objp[1]);
    entry = (ErtsStructEntry*)unsigned_val(defp->entry);

    code_ix = erts_active_code_ix();

    if (entry->definitions[code_ix] == objp[1]) {
        /* Definition is current, go ahead with update */
        for (int i = 0; i < field_count; i++) {
            if (eq(key, defp->fields[i].key)) {
                hp = HAlloc(BIF_P, 2 + field_count);
                sys_memcpy(hp,
                           objp,
                           (2 + field_count) * sizeof(Eterm));
                hp[2 + i] = value;
                BIF_RET(make_struct(hp));
            }
        }
    } else {
        /* Definition is old, upgrade the struct. */
        BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE struct_prototype_read_2(BIF_ALIST_2) {
    /* Struct term, Key */
    ErtsStructDefinition *defp;
    int field_count;
    Eterm obj, *objp;
    Eterm key;

    obj = BIF_ARG_1;
    key = BIF_ARG_2;

    if (!is_struct(obj) ||
        !is_atom(key)) {
        BIF_ERROR(BIF_P, BADARG);
    }

    objp = struct_val(obj);
    field_count = header_arity(objp[0]) - 1;
    defp = (ErtsStructDefinition*)boxed_val(objp[1]);

    for (int i = 0; i < field_count; i++) {
        if (eq(key, defp->fields[i].key)) {
            BIF_RET(objp[2 + i]);
        }
    }

    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE struct_prototype_is_4(BIF_ALIST_4) {
    Eterm obj, module, name, arity;
    ErtsStructDefinition *defp;
    ErtsStructEntry *entry;
    Uint code_ix;

    obj = BIF_ARG_1;
    module = BIF_ARG_2;
    name = BIF_ARG_3;
    arity = BIF_ARG_4;

    if (!is_struct(obj) ||
        !is_atom(module) ||
        !is_atom(name) ||
        !(is_small(arity) && unsigned_val(arity) < MAX_ARG)) {
        BIF_RET(am_false);
    }

    code_ix = erts_active_code_ix();
    entry = erts_struct_find_entry(module, name, unsigned_val(arity), code_ix);
    defp = (ErtsStructDefinition*)(struct_val(obj)[1]);

    ASSERT(is_small(defp->entry));
    if (unsigned_val(defp->entry) == (Uint)entry) {
        BIF_RET(am_true);
    }

    BIF_RET(am_false);
}
