/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
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
#include "erl_record.h"
#include "global.h"

static int
record_cmp(const ErtsExtRecOp *tmpl, const ErtsExtRecOp *obj) {
    if (!(tmpl->code_type == obj->code_type &&
          tmpl->size == obj->size)) {
        return false;
    }

    for (int i = 0; i < tmpl->size; i++) {
        if (tmpl->names_and_dests[i] != obj->names_and_dests[i]) {
            return false;
        }
    }

    return true;
}
