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

-record(b_module, {anno=[],name,exports,attributes,body}).
-record(b_func, {anno=[],args,bs,cnt}).
-record(b_blk, {anno=[],is,last}).
-record(b_set, {anno=[],dst=none,op,args=[]}).

-record(b_ret, {anno=[],arg}).
-record(b_br, {anno=[],bool,succ,fail}).
-record(b_switch, {anno=[],bool,fail,list}).
-record(b_try, {anno=[],var,body,handler}).

-record(b_var, {name}).
-record(b_literal, {val}).
-record(b_remote, {mod,name,arity}).
-record(b_local, {name,arity}).
