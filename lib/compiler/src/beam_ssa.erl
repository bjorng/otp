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
%% Purpose: Type definitions and utilities for the SSA format.

-module(beam_ssa).

-export_type([b_module/0,b_function/0,b_blk/0,b_set/0,
              b_ret/0,b_br/0,b_switch/0,terminator/0,
              b_var/0,b_literal/0,b_remote/0,b_local/0,
              value/0,argument/0,label/0,
              var_name/0,var_base/0]).

-include("beam_ssa.hrl").

-type b_module()   :: #b_module{}.
-type b_function() :: #b_function{}.
-type b_blk()      :: #b_blk{}.
-type b_set()      :: #b_set{}.

-type b_ret()      :: #b_ret{}.
-type b_br()       :: #b_br{}.
-type b_switch()   :: #b_br{}.
-type terminator() :: b_ret() | b_br() | b_switch().

-type b_var()      :: #b_var{}.
-type b_literal()  :: #b_literal{}.
-type b_remote()   :: #b_remote{}.
-type b_local()    :: #b_local{}.

-type value()      :: b_var() | b_literal().
-type argument()   :: b_remote() | b_local().
-type label()      :: non_neg_integer().

-type var_name()   :: var_base() | {var_base(),non_neg_integer()}.
-type var_base()   :: atom() | non_neg_integer().
