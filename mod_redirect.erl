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