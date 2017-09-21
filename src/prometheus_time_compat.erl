%% Copyright Ilya Khaprov <i.khaprov@gmail.com> 2017. All Rights Reserved.
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-module(prometheus_time_compat).

-export([monotonic_time/0,
         convert_time_unit/3,
         os_system_time/1]).

%% ===================================================================
%% API
%% ===================================================================

monotonic_time() ->
  regen(),
  prometheus_time_compat:monotonic_time().

convert_time_unit(Time, From, To) ->
  regen(),
  prometheus_time_compat:convert_time_unit(Time, From, To).

os_system_time(Unit) ->
  regen(),
  prometheus_time_compat:os_system_time(Unit).

%% ===================================================================
%% Private parts
%% ===================================================================

no_monotonic_time() ->
  case erlang:system_info(otp_release) of
    "17" ->
      true;
    [$R | _] ->
      true;
    _ ->
      false
  end.

patch_monotonic_time() ->
  case no_monotonic_time() of
    true ->
      [{match,1,
        {tuple,1,[{var,1,'MS'},{var,1,'S'},{var,1,'US'}]},
        {call,1,{remote,1,{atom,1,erlang},{atom,1,now}},[]}},
       {op,1,'+',
        {op,1,'*',
         {op,1,'+',
          {op,1,'*',{var,1,'MS'},{integer,1,1000000}},
          {var,1,'S'}},
         {integer,1,1000000}},
        {var,1,'US'}}];
    _ ->
      [{call,1,
        {remote,1,{atom,1,erlang},{atom,1,monotonic_time}},
        []}]
  end.

maybe_inject_integer_time_unit() ->
  case no_monotonic_time() of
    true ->
      {function,1,integer_time_unit,1,
       [{clause,1,
         [{atom,1,native}],
         [],
         [{op,1,'*',{integer,1,1000},{integer,1,1000}}]},
        {clause,1,
         [{atom,1,nano_seconds}],
         [],
         [{op,1,'*',
           {op,1,'*',{integer,1,1000},{integer,1,1000}},
           {integer,1,1000}}]},
        {clause,1,
         [{atom,1,micro_seconds}],
         [],
         [{op,1,'*',{integer,1,1000},{integer,1,1000}}]},
        {clause,1,
         [{atom,1,milli_seconds}],
         [],
         [{integer,1,1000}]},
        {clause,1,[{atom,1,seconds}],[],[{integer,1,1}]},
        {clause,1,
         [{var,1,'I'}],
         [[{call,1,{atom,1,is_integer},[{var,1,'I'}]},
           {op,1,'>',{var,1,'I'},{integer,1,0}}]],
         [{var,1,'I'}]},
        {clause,1,
         [{var,1,'BadRes'}],
         [],
         [{call,1,
           {remote,1,{atom,1,erlang},{atom,1,error}},
           [{atom,1,badarg},
            {cons,1,{var,1,'BadRes'},{nil,1}}]}]}]};
    _ ->
      {attribute,1,noop, [noop]}
  end.

%% maybe_inject_floor() ->
%%   case no_monotonic_time() of
%%     true ->
%%       {function,1,floor,1,
%%        [{clause,1,
%%          [{var,1,'Num'}],
%%          [[{call,1,{atom,1,is_float},[{var,1,'Num'}]}]],
%%          [{match,1,
%%            {var,1,'T'},
%%            {call,1,{atom,1,trunc},[{var,1,'Num'}]}},
%%           {match,1,
%%            {var,1,'V'},
%%            {'case',1,
%%             {op,1,'>',
%%              {op,1,'-',{var,1,'Num'},{var,1,'T'}},
%%              {integer,1,0}},
%%             [{clause,1,[{atom,1,true}],[],[{float,1,1.0}]},
%%              {clause,1,[{atom,1,false}],[],[{float,1,0.0}]}]}},
%%           {op,1,'+',{var,1,'T'},{var,1,'V'}}]},
%%         {clause,1,[{var,1,'Num'}],[],[{var,1,'Num'}]}]};
%%     _ ->      
%%       {attribute,1,noop, [noop]}
%%   end.

patch_os_system_time() ->
  case no_monotonic_time() of
    true ->
      [{match,1,
        {tuple,1,[{var,1,'MS'},{var,1,'S'},{var,1,'US'}]},
        {call,1,{remote,1,{atom,1,os},{atom,1,timestamp}},[]}},
       {match,1,
        {var, 1, 'STime'},
        {op,1,'+',
         {op,1,'*',
          {op,1,'+',
           {op,1,'*',{var,1,'MS'},{integer,1,1000000}},
           {var,1,'S'}},
          {integer,1,1000000}},
         {var,1,'US'}}},
       {call,1,{remote,1,{atom,1,prometheus_time_compat},
                {atom,1,convert_time_unit}},
        [{var,1,'STime'}, {atom,1,native}, {var,1,'Unit'}]}];
    _ ->
      [{call,1,
        {remote,1,{atom,1,os},{atom,1,system_time}},
        [{var,1,'Unit'}]}]
  end.

patch_convert_time_unit() ->
  case no_monotonic_time() of
    true ->
      [{match,1,
        {var,1,'FU'},
        {call,1,
         {atom,1,integer_time_unit},
         [{var,1,'FromUnit'}]}},
       {match,1,
        {var,1,'TU'},
        {call,1,
         {atom,1,integer_time_unit},
         [{var,1,'ToUnit'}]}},
       {op,1,'div',
        {op,1,'*',{var,1,'TU'},{var,1,'Time'}},
        {var,1,'FU'}}];
    _ ->
      [{call,1,
        {remote,1,{atom,1,erlang},{atom,1,convert_time_unit}},
        [{var,1,'Time'}, {var,1,'FromUnit'}, {var,1,'ToUnit'}]}]
  end.

regen() ->
  {ok, Module, Binary} =
    compile:forms(
      [{attribute,1,file,
        {"<autogen>",
         1}},
       {attribute,1,module,prometheus_time_compat},
       {attribute,1,export,
        [{monotonic_time,0}]},
       {attribute,1,export,
        [{convert_time_unit,3}]},
       {attribute,1,export,
        [{os_system_time,1}]},
       maybe_inject_integer_time_unit(),
       %% maybe_inject_floor(),
       {function,1,monotonic_time,0,
        [{clause,1,[],[],
          patch_monotonic_time()}]},
       {function,1,convert_time_unit,3,
        [{clause,1,
          [{var,1,'Time'},{var,1,'FromUnit'},{var,1,'ToUnit'}],
          [],
          patch_convert_time_unit()}]},
       {function,1,os_system_time,1,
        [{clause,1,
          [{var,1,'Unit'}],
          [],
          patch_os_system_time()}]},
       {eof,2}]),

  {module, Module} = code:load_binary(Module, "<autogen>", Binary).
