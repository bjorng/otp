/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2025. All Rights Reserved.
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

#include "beam_asm.hpp"

extern "C"
{
#include "erl_struct.h"
}

void BeamModuleAssembler::emit_is_any_native_record(const ArgLabel &Fail,
                                                    const ArgRegister &Src) {
    a.ud2();
}

void BeamModuleAssembler::emit_is_native_record(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgAtom &Module,
                                                const ArgAtom &Name) {
    a.ud2();
}

void BeamModuleAssembler::emit_is_record_accessible(const ArgLabel &Fail,
                                                    const ArgRegister &Src) {
    a.ud2();
}

void BeamModuleAssembler::emit_i_get_record_elements(const ArgLabel &Fail,
                                                     const ArgRegister &Src,
                                                     const ArgWord &Size,
                                                     const Span<ArgVal> &args) {
    a.ud2();
}

void BeamModuleAssembler::emit_i_create_native_record(const ArgWord &local,
                                                      const ArgConstant &Id,
                                                      const ArgRegister &Dst,
                                                      const ArgWord &Live,
                                                      const ArgWord &size,
                                                      const Span<ArgVal> &New) {
    a.ud2();
}

void BeamModuleAssembler::emit_i_update_native_record(const ArgAtom &MODULE,
                                                      const ArgConstant &Id,
                                                      ArgSource const &Src,
                                                      const ArgRegister &Dst,
                                                      const ArgWord &Live,
                                                      const ArgWord &size,
                                                      const Span<ArgVal> &New) {
    a.ud2();
}

void BeamModuleAssembler::emit_get_record_field(const ArgLabel &Fail,
                                                const ArgRegister &Src,
                                                const ArgConstant &Id,
                                                const ArgAtom &Name,
                                                const ArgRegister &Dst) {
    a.ud2();
}
