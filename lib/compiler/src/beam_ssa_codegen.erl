%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% Purpose: Generate BEAM assmebly code from the SSA format.

-module(beam_ssa_codegen).

-export([module/2]).

-include("beam_ssa.hrl").

-spec module(#b_module{}, [compile:option()]) -> {'ok',beam_asm:module_code()}.

module(#b_module{}, _Opts) ->
    io:put_chars("*** WARNING: " ++ ?MODULE_STRING ++
                     " is not implemented yet. ***\n"),
    {ok,nyi}.
