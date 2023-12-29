-module(httpnet).
-export([
    start/0, 
    print/1
]).

-define(SERVER, "find.org.ua").
-define(BIND, "192.168.0.101").
-define(PORT_HTTPS, 8080).
-define(PORT_HTTP, 8081).

%% Запуск https сервера
start() ->
    inets:start(),

    %% ========== HTTPS ========== %%
    {ok, Pid_https} = inets:start(
        httpd,
        [
            {modules, [
                mod_alias,
                %mod_auth,
                mod_esi,
                %mod_actions,
                %mod_cgi,
                %mod_dir,
                mod_get,
                mod_head,
                mod_log,
                mod_disk_log
            ]},

            {port, ?PORT_HTTPS},
            {bind_address, ?BIND},
            {server_name, ?SERVER},

            {socket_type,
                {ssl, [
                    {certfile, "./ssl/fullchain3.pem"},
                    {keyfile, "./ssl/privkey3.pem"}
                ]}},

            {server_root, "./server/"},
            {document_root, "./server/htdocs"},
            {directory_index, ["index.html"]},

            {erl_script_alias, {"/watch", [service]}},
            {erl_script_nocache, true},

            {error_log, "error.log"},
            {security_log, "security.log"},
            {transfer_log, "transfer.log"},
            {log_format, combined},

            {mime_types, [
                {"html", "text/html"},
                {"css", "text/css"},
                {"js", "application/x-javascript"}
            ]},
            {mime_type, "application/octet-stream"}
        ]
    ),
    print([https, Pid_https]),

    %% ========== HTTP ========== %%
    {ok, Pid_http} = inets:start(
        httpd,
        [
            {modules, [
                mod_redirect,
                %mod_alias,
                %mod_auth,
                %mod_esi,
                %mod_actions,
                %mod_cgi,
                %mod_dir,
                mod_get,
                mod_head,
                mod_log,
                mod_disk_log
            ]},

            {port, ?PORT_HTTP},
            {bind_address, ?BIND},
            {server_name, ?SERVER},

            {server_root, "./server80/"},
            {document_root, "./server80/htdocs"},
            {directory_index, ["index.html"]},

            {error_log, "error.log"},
            {security_log, "security.log"},
            {transfer_log, "transfer.log"},
            {log_format, combined},

            {mime_types, [
                {"html", "text/html"},
                {"css", "text/css"},
                {"js", "application/x-javascript"}
            ]},
            {mime_type, "application/octet-stream"}
        ]
    ),
    print([http, Pid_http]).

%% ========== Допоміжні функції ========== %%

print([N | Tail]) ->
    io:format(" ~p", [N]),
    print(Tail);
print([]) ->
    io:format("~n", []);
print(N) ->
    io:format(" ~p~n", [N]).
