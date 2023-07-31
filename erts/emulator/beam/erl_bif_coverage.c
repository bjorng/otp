/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2023. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "beam_load.h"
#include "beam_file.h"

#include "jit/beam_asm.h"

BIF_RETTYPE
get_coverage_mode_1(BIF_ALIST_1)
{
#ifndef BEAMASM
    BIF_ERROR(BIF_P, BADARG);
#else
    ErtsCodeIndex code_ix;
    Module* modp;
    const BeamCodeHeader* hdr;

    if (is_not_atom(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);
    if (modp == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    hdr = modp->curr.code_hdr;
    if (hdr == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    switch (hdr->coverage_mode) {
    case ERTS_COV_FUNCTION_COVERAGE:
        BIF_RET(am_function);
    case ERTS_COV_LINE_COVERAGE:
        BIF_RET(am_line_coverage);
    case ERTS_COV_LINE_COUNTERS:
        BIF_RET(am_line_counters);
    case ERTS_COV_NONE:
        BIF_RET(am_none);
    default:
        ASSERT(0);
        BIF_RET(am_none);
    }
#endif
}

BIF_RETTYPE
get_function_coverage_1(BIF_ALIST_1)
{
#ifndef BEAMASM
    BIF_ERROR(BIF_P, BADARG);
#else
    ErtsCodeIndex code_ix;
    Module* modp;
    const BeamCodeHeader* hdr;
    Eterm* hp;
    Eterm* hend;
    Eterm res;
    Eterm tmp;
    ssize_t i;
    Uint alloc_size;

    if (is_not_atom(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);
    if (modp == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    hdr = modp->curr.code_hdr;
    if (hdr == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    switch (hdr->coverage_mode) {
    case ERTS_COV_FUNCTION_COVERAGE: {
        byte* p = hdr->coverage;

        res = NIL;
        alloc_size = (3 + 3 + 2) * hdr->num_functions;
        hp = HAlloc(BIF_P, alloc_size);
        hend = hp + alloc_size;
        for (i = (ssize_t)(hdr->num_functions - 1); i >= 0; i--) {
            if (hdr->functions[i]->mfa.function == am_module_info &&
                hdr->functions[i]->mfa.arity <= 1) {
                /* For consistency with function coverage in line
                 * counters mode, don't include module_info/0 and
                 * module_info/1 in the list. */
                continue;
            }

            tmp = TUPLE2(hp, hdr->functions[i]->mfa.function,
                         make_small(hdr->functions[i]->mfa.arity));
            hp += 3;
            tmp = TUPLE2(hp, tmp, p[i] ? am_true : am_false);
            hp += 3;
            res = CONS(hp, tmp, res);
            hp += 2;
        }
        HRelease(BIF_P, hend, hp);
        BIF_RET(res);
    }
    case ERTS_COV_LINE_COVERAGE:
    case ERTS_COV_LINE_COUNTERS: {
        const BeamCodeLineTab *lt = hdr->line_table;
        Eterm covered;

        res = NIL;
        alloc_size = (3 + 3 + 2) * hdr->num_functions;
        hp = HAlloc(BIF_P, alloc_size);
        hend = hp + alloc_size;
        for (i = (ssize_t)(hdr->num_functions - 1); i >= 0; i--) {
            int index = lt->func_tab[i] - lt->func_tab[0];
            int high = lt->func_tab[i+1] - lt->func_tab[0];

            if (hdr->functions[i]->mfa.function == am_module_info &&
                hdr->functions[i]->mfa.arity <= 1) {
                /* There are no executable_line instruction in the
                 * module_info/0 and module_info/1 functions, and
                 * therefore they would never be shown to be covered.
                 * Better not include in them in the list. */
                continue;
            }

            if (hdr->coverage_mode == ERTS_COV_LINE_COVERAGE) {
                for (const byte *p = hdr->coverage; index < high && !p[index]; ) {
                    index++;
                }
            } else {
                ASSERT(hdr->coverage_mode == ERTS_COV_LINE_COUNTERS);
                for (const Uint *p = hdr->coverage; index < high && !p[index]; ) {
                    index++;
                }
            }

            covered = index < high ? am_true : am_false;

            tmp = TUPLE2(hp, hdr->functions[i]->mfa.function,
                         make_small(hdr->functions[i]->mfa.arity));
            hp += 3;
            tmp = TUPLE2(hp, tmp, covered);
            hp += 3;
            res = CONS(hp, tmp, res);
            hp += 2;
        }
        HRelease(BIF_P, hend, hp);
        BIF_RET(res);
    }
    case ERTS_COV_NONE:
    default:
        BIF_ERROR(BIF_P, BADARG);
    }
#endif
}

BIF_RETTYPE
get_line_coverage_1(BIF_ALIST_1)
{
#ifndef BEAMASM
    BIF_ERROR(BIF_P, BADARG);
#else
    ErtsCodeIndex code_ix;
    Module* modp;
    const BeamCodeHeader* hdr;
    const BeamCodeLineTab *lt;
    Eterm* hp;
    Eterm* hend;
    Eterm tmp;
    Eterm res;
    ssize_t i;
    unsigned location;
    Uint alloc_size;
    byte coverage_mode;

    if (is_not_atom(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);
    if (modp == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    hdr = modp->curr.code_hdr;
    if (hdr == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    coverage_mode = hdr->coverage_mode;

    switch (coverage_mode) {
    case ERTS_COV_LINE_COVERAGE:
    case ERTS_COV_LINE_COUNTERS:
        break;
    default:
        BIF_ERROR(BIF_P, BADARG);
    }

    lt = hdr->line_table;

    alloc_size = (3 + 2) * hdr->line_coverage_len;
    hp = HAlloc(BIF_P, alloc_size);
    hend = hp + alloc_size;
    res = NIL;
    for (i = hdr->line_coverage_len - 1; i >= 0; i--) {
        Eterm coverage = am_error;
        void* coverage_array = hdr->coverage;

        if (!hdr->line_coverage_valid[i]) {
            continue;
        }
        if (lt->loc_size == 2) {
            location = lt->loc_tab.p2[i];
        } else {
            ASSERT(lt->loc_size == 4);
            location = lt->loc_tab.p4[i];
        }
        if (location == LINE_INVALID_LOCATION) {
            continue;
        }
        if (coverage_mode == ERTS_COV_LINE_COVERAGE) {
            byte* p = coverage_array;
            coverage = p[i] ? am_true : am_false;
        } else if (coverage_mode == ERTS_COV_LINE_COUNTERS) {
            Uint* p = coverage_array;
            coverage = make_small(MIN(p[i], MAX_SMALL));
        }
        tmp = TUPLE2(hp, make_small(LOC_LINE(location)), coverage);
        hp += 3;
        res = CONS(hp, tmp, res);
        hp += 2;
    }
    HRelease(BIF_P, hend, hp);
    BIF_RET(res);
#endif
}

BIF_RETTYPE
reset_coverage_1(BIF_ALIST_1)
{
#ifndef BEAMASM
    BIF_ERROR(BIF_P, BADARG);
#else
    ErtsCodeIndex code_ix;
    Module* modp;
    const BeamCodeHeader* hdr;

    if (is_not_atom(BIF_ARG_1)) {
        BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
    modp = erts_get_module(BIF_ARG_1, code_ix);
    if (modp == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }
    hdr = modp->curr.code_hdr;
    if (hdr == NULL) {
        BIF_ERROR(BIF_P, BADARG);
    }

    switch (hdr->coverage_mode) {
    case ERTS_COV_FUNCTION_COVERAGE:
        sys_memset(hdr->coverage, 0, hdr->num_functions);
        break;
    case ERTS_COV_LINE_COVERAGE:
        sys_memset(hdr->coverage, 0, hdr->line_coverage_len);
        break;
    case ERTS_COV_LINE_COUNTERS:
        sys_memset(hdr->coverage, 0, hdr->line_coverage_len * sizeof(Uint));
        break;
    default:
        BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_ok);
#endif
}
