% 
% Copyright (C) 2023 TARAKHOMYN YURIY IVANOVYCH
% All rights reserved.
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
% 
%
% 
% Автор:    Тарахомин Юрій Іванович
% Адреса:   Україна, м. Львів
% Сайт:     accounting.org.ua
% 


%
% Модуль для перенаправлення запитів з http на https
%

-module(mod_redirect).
-export([do/1]).
-include_lib("inets/include/httpd.hrl").

do(#mod{request_uri   = RequestUri,
        parsed_header = Header}) ->
    Host = get_host(Header),
    {proceed, [{response, {301, "Location: https://" ++ Host  ++ RequestUri ++ "\r\nConnection: close\r\n\r\n" }}]}.

get_host([]) -> 
    "";
get_host([{Key, Value} | Header]) ->
    if
        Key =:= "host" ->
            re:replace(Value, "^www.", "", [{return, list}]);
        true ->
            get_host(Header)
    end.