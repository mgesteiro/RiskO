PROGRAM Tonteria_DLL;
USES
  WinCrt,Winsock;
VAR
  Resultado : integer;
  EuWSData  : WSAData;
BEGIN
  Writeln('Tonter�a padre 1');
  Resultado:=WSAStartup ($0101, @EuWSData);
  Writeln('Tonter�a padre 2');
  Writeln(Resultado);
  readln;
  WSACleanup;
END.
