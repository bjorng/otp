/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
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

#ifndef __ERL_RECORD_H__
#define __ERL_RECORD_H__

#include "sys.h"
#include "code_ix.h"
#include "erl_process.h"

/* Native record entry. */
typedef struct {
    Eterm module;
    Eterm name;

    /* Literal-tagged CONS pointers. The head of the CONS holds to
     * pointer to the canonical ErtsRecordDefinition for each code
     * generation, while the tail holds the default values. */
    Eterm definitions[ERTS_ADDRESSV_SIZE];
} ErtsRecordEntry;

/* Native record definitions are tagged as tuples to simplify the
 * GC. They should never be presented to the user. */
typedef struct {
    Eterm thing_word;

    /* Tuple mapping from original field order to sorted field order. */
    Eterm field_order;

    /* Here is the hash value for the fields that follow. This hash
     * value is part of the phash2 hash. If more fields are to be added
     * to this struct, they should appear before this field. */
    Eterm hash;

    Eterm module;
    Eterm name;
    Eterm is_exported;

    /* The keys for the native record in atom index order. (During
     * calculation of the hash, the keys are in definition order.) */
    Eterm keys[];
} ErtsRecordDefinition;

/* A native-record value (instance). */
typedef struct {
    Eterm thing_word;

    /* Boxed-tagged ErtsRecordDefinition* */
    Eterm record_definition;

    Eterm values[];
} ErtsRecordInstance;

#define record_field_count(x)                                           \
    (header_arity(*record_val(x)) - sizeof(ErtsRecordInstance)/sizeof(Eterm) + 1)

void erts_record_init_table(void);

void erts_record_module_delete(Eterm module);

Eterm erts_canonical_record_def(ErtsRecordDefinition *defp);

ErtsRecordEntry *erts_record_put(Eterm module,
                                 Eterm name);

bool erl_is_native_record(Eterm Src, Eterm Mod, Eterm Name);
bool erl_is_record_accessible(Eterm src, Eterm Mod);

Eterm erl_get_record_field(Process* p, Eterm src, Eterm mod, Eterm id, Eterm field);

bool erl_get_record_elements(Process* P, Eterm* reg, Eterm src,
                             Uint size, const Eterm* new_p);

void erts_struct_start_staging(void);
void erts_struct_end_staging(int commit);

Eterm erl_create_native_record(Process* p, Eterm* reg, Eterm id,
                               Uint live, Uint size, const Eterm* new_p);
Eterm erl_update_native_record(Process* c_p, Eterm* reg, Eterm src,
                               Uint live, Uint size, const Eterm* new_p);

#endif /* __ERL_RECORD_H__ */
