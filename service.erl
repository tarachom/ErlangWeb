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
% Модуль для отримання даних з локального вебсервера
%

-module(service).

-export([
    search/3,
    news/3,
    personality/3,
    sitemap_news/3, 
    sitemap_personality/3,
    about/3,
    feedback/3,
    post_request/5
]).

-define(WEBHOST, "http://localhost:8082/").

search(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "search", "text/html").

% Новини
news(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "news", "text/html").

% Особистості
personality(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "personality", "text/html").

% Sitemap news
sitemap_news(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "sitemap-news", "text/xml").

% Sitemap personality
sitemap_personality(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "sitemap-personality", "text/xml").

% Про проект
about(SessionID, Env, _Input) ->
    get_request(SessionID, Env, "about", "text/html").

% Зворотній зв'язок
feedback(SessionID, _Env, Input) ->
    post_request(SessionID, _Env, "feedback", "text/html", Input).

% Вибірка даних
get_request(SessionID, Env, Service, ContentType) ->
    QueryString = query_string(Env),
    PathInfo = path_info(Env),
    Request = {?WEBHOST ++ Service ++ QueryString ++ PathInfo, []},
    {R, Result} = httpc:request(get, Request, [], []),
    if
        R =:= ok ->
            {{_Protocol, Code, _Info}, _Headers, Body} = Result,
            if
                Code =:= 200 ->
                    mod_esi:deliver(SessionID, [
                        "Content-Type:" ++ ContentType, "\r\n\r\n", io_lib:format("~s~n", [Body])
                    ]);
                Code =:= 404 ->
                    % Перенаправити на стартову сторінку сервісу
                    mod_esi:deliver(SessionID, ["Location: /watch/service/" ++ Service, "\r\n\r\n"]);
                true ->
                    % У всіх інших випадках на початкову сторінку (!Треба зробити окрему сторінку для помилок)
                    mod_esi:deliver(SessionID, ["Location: /", "\r\n\r\n"])
            end;
        true ->
            % У випадку помилки перенаправити на сторінку що сервіс тимчасово не працює
            mod_esi:deliver(SessionID, ["Location: /", "\r\n\r\n"])
    end.

% Відправка даних
post_request(SessionID, _Env, Service, ContentType, Input) ->
    Request = {?WEBHOST ++ Service, [], "application/x-www-form-urlencoded", Input},
    {R, Result} = httpc:request(post, Request, [], []),
    if
        R =:= ok ->
            {{_Protocol, Code, _Info}, _Headers, Body} = Result,
            if
                Code =:= 200 ->
                    mod_esi:deliver(SessionID, [
                        "Content-Type:" ++ ContentType, "\r\n\r\n", io_lib:format("~s~n", [Body])
                    ]);
                true ->
                    % У всіх інших випадках на початкову сторінку (!Треба зробити окрему сторінку для помилок)
                    mod_esi:deliver(SessionID, ["Location: /", "\r\n\r\n"])
            end;
        true ->
            % У випадку помилки перенаправити на сторінку що сервіс тимчасово не працює
            mod_esi:deliver(SessionID, ["Location: /", "\r\n\r\n"])
    end.

path_info([]) -> 
    "";
path_info([{Key, Value} | Env]) ->
    if
        Key =:= 'path_info' ->
            "/" ++ Value;
        true ->
            path_info(Env)
    end.

query_string([]) -> 
    "";
query_string([{Key, Value} | Env]) ->
    if
        Key =:= 'query_string' ->
            "?" ++ Value;
        true ->
            query_string(Env)
    end.
