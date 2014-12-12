%% This is the application resource file (.app file) for the 'base'
%% application.
{application, chat, 
 [{description, "a server for chat"},
  {vsn, "3.0"},
  {modules, [chat_sup,chat_app]},	
  {registered,[]},
  {applications, [kernel,stdlib]},
  {mod, {chat_app,[]}},
  {start_phases, []}
 ]}.
