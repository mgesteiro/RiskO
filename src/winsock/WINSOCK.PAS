unit WINSOCK;

{ Winsock.h file for Borland Pascal
  Conversion by Marc B. Manza
             Center for Applied Large-Scale Computing
  Requires Winsock.pas and Winsock.inc

  Send any comments/change requests/etc. to:
  manza@flash.poly.edu }

interface

uses WIN31, WinTypes, WinProcs;

{$I winsock.inc }

{ Library Functions }

  function accept (s : SOCKET; addr : sockaddr; addrlen : integer) : SOCKET;
  function bind (s : SOCKET; addr : sockaddr; namelen : integer) : integer;
  function closesocket (s : SOCKET) : integer;
  function connect (s : SOCKET; name : sockaddr; namelen : integer) : integer;
  function ioctlsocket (s : SOCKET; cmd : longint; argp : u_long) : integer;
  function getpeername (s : SOCKET; name : sockaddr; namelen : integer) : integer;
  function getsockname (s : SOCKET; name : sockaddr; namelen : integer) : integer;
  function getsockopt (s : SOCKET; level, optname : integer; optval : PChar; optlen : integer) : integer;
  function htonl (hostlong : u_long) : u_long;
  function htons (hostshort : u_short) : u_short;
  function inet_addr (cp : PChar) : PIn_Addr;  { in_addr }
  function inet_ntoa (inaddr : in_addr) : PChar;
  function listen (s : SOCKET; backlog : integer) : integer;
  function ntohl (netlong : u_long) : u_long;
  function ntohs (netshort : u_short) : u_short;
  function recv (s : SOCKET; buf : PChar; len, flags : integer) : integer;
  function recvfrom (s : SOCKET; buf : PChar; len, flags : integer; from : sockaddr; fromlen : integer) : integer;
  function select (nfds : integer; readfds, writefds, exceptfds : fd_set; timeout : timeval) : longint;
  function send (s : SOCKET; buf : PChar; len, flags : integer) : integer;
  function sendto (s : SOCKET; buf : PChar; len, flags : integer; addrto : sockaddr; tolen : integer) : integer;
  function setsockopt (s : SOCKET; level, optname : integer; optval : PChar; optlen : integer) : integer;
  function shutdown (s : SOCKET; how : integer) : integer;
  function _socket (af, struct, protocol : integer) : SOCKET;
  function gethostbyaddr (addr : PChar; len, struct : integer) : PHostEnt; { hostent }
  function gethostbyname (name : PChar) : PHostEnt; { hostent }
  function getservbyport (port : integer; proto : PChar) : PServEnt; { servent }
  function getservbyname (name, proto : PChar) : PServEnt; { servent }
  function getprotobynumber (proto : integer) : PProtoEnt; { protoent }
  function getprotobyname (name : PChar) : PProtoEnt; { protoent }
  function WSAStartup (wVersionRequired : word; lpWSData : LPWSADATA) : integer;
  function WSACleanup : integer;
  procedure WSASetLastError (iError : integer);
  function WSAGetLastError : integer;
  function WSAIsBlocking : BOOL;
  function WSAUnhookBlockingHook : integer;
  function WSASetBlockingHook (lpBlockFunc : TFarProc) : TFarProc;
  function WSACancelBlockingCall : integer;
  function WSAAsyncGetServByName (HWindow : HWND; wMsg : u_int; name, proto, buf : PChar; buflen : integer) : THandle;
  function WSAAsyncGetServByPort ( HWindow : HWND; wMsg, port : u_int; proto, buf : PChar; buflen : integer) : THandle;
  function WSAAsyncGetProtoByName (HWindow : HWND; wMsg : u_int; name, buf : PChar; buflen : integer) : THandle;
  function WSAAsyncGetProtoByNumber (HWindow : HWND; wMsg : u_int; number : integer;
                                      buf : PChar; buflen : integer) : THandle;
  function WSAAsyncGetHostByName (HWindow : HWND; wMsg : u_int; name, buf : PChar; buflen : integer) : THandle;
  function WSAAsyncGetHostByAddr (HWindow : HWND; wMsg : u_int; addr : PChar; len, struct : integer;
                                  buf : PChar; buflen : integer) : THandle;
  function WSACancelAsyncRequest (hAsyncTaskHandle : THandle) : integer;
  function WSAAsyncSelect (s : SOCKET; HWindow : HWND; wMsg : u_int; lEvent : longint) : integer;


function WSAMakeSyncReply (Buflen, Error : Word) : LongInt;
function WSAMakeSelectReply (Event, Error : Word) : LongInt;
function WSAGetAsyncBuflen (Param : LongInt) : Word;
function WSAGetAsyncError (Param : LongInt) : Word;
function WSAGetSelectEvent (Param : LongInt) : Word;
function WSAGetSelectError (Param : LongInt) : Word;

implementation

var
   fdSet : fd_set;

function FIONBIO (x, y : integer; t : u_long) : u_long;
begin
     FIONBIO := (IOC_IN OR ((IOCPARM_MASK shl 16) OR (x SHL 8) OR (Y)));
end;

function WSAMakeSyncReply;
begin
     WSAMakeSyncReply := MakeLong (Buflen, Error);
end;

function WSAMakeSelectReply;
begin
     WSAMakeSelectReply := MakeLong (Event, Error);
end;

function WSAGetAsyncBuflen;
begin
     WSAGetAsyncBuflen := LOWORD(Param);
end;

function WSAGetAsyncError;
begin
     WSAGetAsyncError := HIWORD(Param);
end;

function WSAGetSelectEvent;
begin
     WSAGetSelectEvent := LOWORD(Param);
end;

function WSAGetSelectError;
begin
     WSAGetSelectError := HIWORD(Param);
end;

function accept;            external    'WINSOCK' index 1;
function bind;              external    'WINSOCK' index 2;
function closesocket;       external    'WINSOCK' index 3;
function connect;           external    'WINSOCK' index 4;
function ioctlsocket;       external    'WINSOCK' index 12;
function getpeername;       external    'WINSOCK' index 5;
function getsockname;       external    'WINSOCK' index 6;
function getsockopt;        external    'WINSOCK' index 7;
function htonl;             external    'WINSOCK' index 8;
function htons;             external    'WINSOCK' index 9;
function inet_addr;         external    'WINSOCK' index 10;
function inet_ntoa;         external    'WINSOCK' index 11;
function listen;            external    'WINSOCK' index 13;
function ntohl;             external    'WINSOCK' index 14;
function ntohs;             external    'WINSOCK' index 15;
function recv;              external    'WINSOCK' index 16;
function recvfrom;          external    'WINSOCK' index 17;
function select;            external    'WINSOCK' index 18;
function send;              external    'WINSOCK' index 19;
function sendto;            external    'WINSOCK' index 20;
function setsockopt;        external    'WINSOCK' index 21;
function shutdown;          external    'WINSOCK' index 22;
function _socket;            external    'WINSOCK' index 23;

function gethostbyaddr;     external    'WINSOCK' index 51;
function gethostbyname;     external    'WINSOCK' index 52;
function getprotobyname;    external    'WINSOCK' index 53;
function getprotobynumber;  external    'WINSOCK' index 54;
function getservbyname;     external    'WINSOCK' index 55;
function getservbyport;     external    'WINSOCK' index 56;

function WSAAsyncSelect;    external    'WINSOCK' index 101;
function WSAAsyncGetHostByAddr; external 'WINSOCK' index 102;
function WSAAsyncGetHostByName; external 'WINSOCK' index 103;
function WSAAsyncGetProtoByNumber; external 'WINSOCK' index 104;
function WSAAsyncGetprotoByName; external 'WINSOCK' index 105;
function WSAAsyncGetServByPort; external 'WINSOCK' index 106;
function WSAAsyncGetServByName; external 'WINSOCK' index 107;
function WSACancelAsyncRequest; external 'WINSOCK' index 108;
function WSASetBlockingHook; external    'WINSOCK' index 109;
function WSAUnhookBlockingHook; external 'WINSOCK' index 110;
function WSAGetLastError;    external    'WINSOCK' index 111;
procedure WSASetLastError;   external    'WINSOCK' index 112;
function WSACancelBlockingCall; external 'WINSOCK' index 113;
function WSAIsBlocking;     external     'WINSOCK' index 114;
function WSAStartup;        external     'WINSOCK' index 115;
function WSACleanup;        external     'WINSOCK' index 116;

end.