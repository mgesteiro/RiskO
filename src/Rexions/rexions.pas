(**************************************************************************)
(*  APLICACI�N:  Rexi�ns                                                  *)
(*  PROP�SITO:   Utilidade para a edici�n e creaci�n de rexi�ns (HRegion) *)
(*  RESULTADOS:  Arquivos de rexi�ns de formato propio (colecci�ns de pun-*)
(*               tos + cabeceira) con rexi�ns para importar dende o RiskO!*)
(*                                                                        *)
(*  AUTOR:       Miguel Gesteiro (C)..........                            *)
(**************************************************************************)
PROGRAM Rexions;
{$R rexions.res}

USES
  (* Borland *)
  WinTypes,WinProcs,OMemory,CommDlg,
  (* Propias *)
  BMP;


(*########################################################################*)
(*                     CONSTANTES, TIPOS E VARIABLES                      *)
(*########################################################################*)
CONST
  (* Constantes sobre a aplicaci�n *)
  Clase_Nome='ClasePropia';(* O nome da clase de VP                 *)
  VP_Nome   ='Rexi�ns';    (* Nome da vent� principal               *)
  App_Ver_P ='0.0';        (* N� Principal da versi�n da aplicaci�n *)
  App_Ver_M ='0.1';        (* N� Menor da versi�n da aplicaci�n     *)
  (* Constantes principais *)
  Max_Rex   =60;  (* N� m�ximo de rexi�ns cas que se pode traballar *)
  Max_P_Rex =450; (* N� m�ximo de puntos por rexi�n                 *)
  AnchoCl   =635; (* Ancho do �rea cliente da vent� principal       *)
  AltoCl    =410; (* Alto do �rea cliente da vent� principal        *)
  (* Identificadores das opci�ns do men� *)
  ID_Novo    =101; (* Identificador do men� Arquivo|Novo          *)
  ID_Abrir   =102; (* Identificador do men� Arquivo|Abrir         *)
  ID_Gardar  =103; (* Identificador do men� Arquivo|Gardar        *)
  ID_GardarC =104; (* Identificador do men� Arquvio|Gardar como   *)
  ID_Sair    =105; (* Identificador do men� Arquivo|Sair          *)
  ID_Desfacer=201; (* Identificador do men� Rexi�n|Desfacer       *)
  ID_Nova    =202; (* Identificador do men� Rexi�n|Nova           *)
  ID_Eliminar=203; (* Identificador do men� Rexi�n|Eliminar       *)
  ID_Numerar =204; (* Identificador do men� Rexi�n|Numerar        *)
  ID_Fondo   =301; (* Identificador do men� Especial|Cargar fondo *)
  ID_Contido =501; (* Identificador do men� Axuda|Contido         *)
  ID_Sobre   =502; (* Identificador do men� Axuda|Sobre           *)
  (* Identificadores de Di�logos *)
  CID_Nome   =101; (* Identificador do control "nome" do di�logo Datos_Rex *)
  CID_Numero =102; (* Identificador do control "numero" do mesmo di�logo   *)
  {ID_Dialogo =300; (* Identificador do men� di�logo *)}
  (* outras constantes *)
  BMType     =$4D42; (* ='BM'=bitmap *)

TYPE
  (* Clave ******************************************************)
  (*  T : tipo xen�rico. Non ten un prop�sito espec�fico.       *)
  (*  TF: tipo ficheiro. Est� pensado para usalo con ficheiros. *)
  (*  TM: tipo memoria. Est� pensado para usalo en memoria.     *)
  (*  TP: tipo punteiro. Punteiro a outro tipo declarado        *)
  (**************************************************************)
  (* Cabeceira dun ficheiro de rexi�ns *)
  TFCab_Fich  =RECORD
                 File_ID:array[0..7] of char; (* identificador de ficheiro *)
                 Descripcion:array[0..255] of char; (* finalizado en #0    *)
                 Version:array[0..6] of char; (* ver. do programa usado    *)
                 Num_Rexions:integer; (* n�mero de rexi�ns no arquivo      *)
                 Max_Ptos:integer; (* n� m�ximo de ptos por rexi�n         *)
                 Tot_Ptos:longint  (* n� total de ptos (x,y) do ficheiro   *)
               END;
  (* Cabeceira dunha rexi�n *)
  TFCab_Rexion=RECORD
                 Nome:array[0..40] of char; (* nome da rexi�n. Fin. en #0 *)
                 Orden:byte; (* n�mero ordinal de la regi�n *)
                 Num_ptos:integer (* n�mero de ptos (x,y) da rexi�n *)
               END;
  (* Punto (x,y) *)
  TPunto      =TPoint; (* x e y integers *)
  (* Punteiro a un punto (x,y) *)
  TPPunto     =^TPoint; (* x e y integers *)
  (* Cabeceira de rexi�n + conxunto de puntos (x,y) *)
  TMRexion    =RECORD
                 Inf_Rexion:TFCab_Rexion; (* datos sobre a rexi�n *)
                 Contador:integer;        (* punto actual *)
                 Mango:HRgn;              (* o handle da rexi�n *)
                 Ult_Punto: TPPunto;      (* punteiro � �ltimo punto *)
                 Dat_Rexion:TPPunto       (* conxunto de puntos *)
               END;
  (* Estado do programa, para control interno *)
  TEstado = (Normal,Debuxando,Flasheando);

VAR
  (* Variables fundamentais do programa *)
  EstaInstancia   : THandle;   (* para renombrar a variable Windows *)
  InstanciaPrevia : THandle;   (* para renombrar a variable Windows *)
  ClaseVP         : TWndClass; (* estruct. clase_ventana para a VP *)
  VP              : HWnd;      (* Vent� Principal *)
  AnchoVP,AltoVP  : Integer;   (* alto e ancho da vent� principal *)
  PosiX,PosiY     : Integer;   (* posici�ns x e y da vent� principal *)
  Mensaxe         : TMsg;      (* Estructura de Mensaxe *)
  VPDC            : HDC;       (* O Device Context da clase da vent� VP *)
  (* Outras variables globais *)
  (* variables de rexi�ns *)
  A_Rexion        : Array[1..Max_Rex] of TMRexion; (* conx. de rexi�ns *)
  Actual          : integer; (* rexi�n ca que se est� traballando *)
  (* variables de control *)
  Estado          : TEstado; (* variable de estado do programa *)
  Flash           : byte; (* para contar y controlar el flash *)
  Resposta        : integer; (* resposta dos di�logos *)
  Modificado      : boolean; (* indica se se fixo algunha modificaci�n *)
  (* variables do common dialog *)
  AbrirFicheiro   : TOpenFilename; (* Estructura do common dialog *)
  CaptionDialogo  : string[128];   (* cadea para o t�tulo do common dialog *)
  FiltroFicheiro  : string[128];   (* cadea con filtros para o common dialog *)
  NomeFicheiro    : string[128];   (* ruta+nome dun ficheiro *)
  (* variables variadas :) *)
  OBitMap         : HBitmap;       (* O bitmap que debuxamos *)



(*########################################################################*)
(*                 FUNCI�NS E PROCEDEMENTOS XERAIS                        *)
(*########################################################################*)
(*************************************************************************)
(*       Procedemento encargado de iniciar algunhas variables            *)
(*************************************************************************)
PROCEDURE Inicia_Var;
  BEGIN
    Estado:=Normal; (* estado da aplicaci�n *)
    Flash:=0; (* contador do timer para saber cando flashear unha rexi�n *)
    Resposta:=0; (* valor devolto por un di�logo *)
    Actual:=0; (* �ndice da rexi�n actual ca que se traballa *)
    Modificado:=FALSE; (* non hai modificaci�ns todav�a *)
    (* 1) iniciar o file_id da variable do tipo TFCab_Rexi�n *)
  END; (* Procedure Inicia_Var *)

(*************************************************************************)
(*   Procedemento encargado de iniciar a variable "A_Rexion"             *)
(*************************************************************************)
PROCEDURE Inicia_A_Rexion;
  VAR
    i:integer;
  BEGIN
    FOR i:=1 TO Max_Rex DO WITH A_Rexion[i] DO
      BEGIN
        Contador:=0; (* punto actual *)
        Mango:=0;    (* handle da rexi�n *)
        Ult_Punto:=NIL;  (* punteiro � �ltimo punto da rexi�n *)
        Dat_Rexion:=NIL; (* punteiro � conxunto de puntos *)
      END
  END; (* Procedure Inicializa_A_Rexion *)

(*************************************************************************)
(*   Procedemento encargado de crear o espacio para a rexi�n indicada    *)
(*************************************************************************)
PROCEDURE Crea_A_Rexion(PActual:integer);
  BEGIN
    WITH A_Rexion[PActual] DO
      BEGIN
        (* inicializaci�n do subrecord "Inf_Rexion" *)
        LStrCpy(@(Inf_Rexion.Nome),'');
        Inf_Rexion.Orden:=PActual;
        Inf_Rexion.Num_ptos:=0;
        (* resto de campos *)
        Contador:=0;
        (*Mango:=0 isto � redundante, xa o fai Inicializa_A_Rexion *)
        Dat_Rexion:=MemAlloc(Max_P_Rex*SizeOf(TPunto));
        Ult_Punto:=Dat_Rexion
      END (* With A_Rexion[Actual] *)
  END; (* Procedure Crea_A_Rexion *)

(*************************************************************************)
(*  Procedemento encargado de destruir o espacio da rexi�n indicada      *)
(*************************************************************************)
PROCEDURE Destrue_A_Rexion(PActual:integer);
  BEGIN
    WITH A_Rexion[PActual] DO
      BEGIN
        IF Mango<>0 THEN DeleteObject(Mango);
        Mango:=0;
        IF Dat_Rexion<>NIL THEN FreeMem(Dat_Rexion,Max_P_Rex*SizeOf(TPunto));
        Dat_Rexion:=NIL;
        Ult_Punto:=NIL
      END (* With A_Rexion[Actual] *)
  END; (* Procedure Crea_A_Rexion *)

(*************************************************************************)
(*   Procedemento encargado de liberar a variable "A_Rexion"             *)
(*************************************************************************)
PROCEDURE Libera_A_Rexion;
  VAR
    i:integer;
  BEGIN
    FOR i:=1 TO Max_Rex DO Destrue_A_Rexion(i)
  END; (* Procedure Inicializa_A_Rexion *)

(*************************************************************************)
(*   Procedemento para pintar linhas a partires dos puntos das rexi�ns   *)
(*************************************************************************)
PROCEDURE PintarUltimaLinha(PUlt_Punto:TPPunto; PBorrar:boolean);
  BEGIN
    IF PBorrar THEN SetROP2(VPDC,R2_NOT) (* Debuxamos invertindo *)
               ELSE SetROP2(VPDC,R2_COPYPEN); (* Color do pincel *)
    Dec(PUlt_Punto);
    MoveTo(VPDC,PUlt_Punto^.X,PUlt_Punto^.Y);
    Inc(PUlt_Punto);
    LineTo(VPDC,PUlt_Punto^.X,PUlt_Punto^.Y);
    SetPixel(VPDC,PUlt_Punto^.X,PUlt_Punto^.Y,$000000FF);
    Dec(PUlt_Punto);
    SetPixel(VPDC,PUlt_Punto^.X,PUlt_Punto^.Y,$000000FF)
  END; (* Procedure PintarUltimaLinha *)

(*************************************************************************)
(*   Procedemento que pinta un Bitmap na vent� principal                 *)
(*************************************************************************)
PROCEDURE PintaOBitmap(PVPDC:HDC; POBitmap:HBitmap);
  VAR
    OBmpInfo : TBitmap;  (* Cabeceira dun bitmap *)
    ProviDC  : HDC;      (* Device context para copiar o bitmap *)
 BEGIN
    GetObject(POBitmap,sizeOf(OBmpInfo),@OBmpInfo); (* obtemos a cabeceira do bitmap *)
    ProviDC:=CreateCompatibleDC(PVPDC);  (* facemos un sitio para seleccionalo *)
    DeleteObject(SelectObject(ProviDC,POBitmap));  (* seleccionamos o bitmap *)
    BitBlt(PVPDC,0,0,OBmpInfo.bmWidth,OBmpInfo.bmHeight,ProviDC,0,0,SrcCopy); (* o compiamos *)
    DeleteDC(ProviDC) (* liberamos o recurso *)
  END; (* Procedure PintaOBitmap *)

(*************************************************************************)
(*  Procedemento que repinta a vent� principal en resposta a un WM_paint *)
(*************************************************************************)
PROCEDURE RepintarVP;
  VAR
    StruPintar : TPaintStruct;
  BEGIN
    BeginPaint(VP,StruPintar);
    IF OBitmap<>0 THEN PintaOBitmap(VPDC,OBitmap);
    EndPaint(VP,StruPintar);
  END; (* Procedure RepintarVP *)

(*************************************************************************)
(*   Procedemento que enche os campos da estructura "AbrirFicheiro"      *)
(*************************************************************************)
PROCEDURE EncheAbrirFicheiro(PNomeFicheiro,PFiltro,PCaption,PExtension:PChar);
  BEGIN
    PNomeFicheiro^:=#0; (* inicializaci�n necesaria *)
    WITH AbrirFicheiro DO
      BEGIN
        hWndOwner:=VP;  (* vent� dona do common dialog *)
        hInstance:=EstaInstancia; (* instancia para o dialog *)
        lpstrFilter:=PFiltro; (* conxunto de filtros *)
        lpstrCustomFilter:=nil;  (* sen filtros custom *)
        nMaxCustFilter:=0; (* m�nimo obligado � 40 se lpstrCustomFilter<>nil*)
        nFilterIndex:=1; (* usamos o primeiro filtro da lista *)
        lpstrFile:=@NomeFicheiro[1]; (* ruta+nome completo do ficheiro *)
        nMaxFile:=128; (* Tama�o do buffer lpstrFile *)
        lpstrFileTitle:=nil; (* buffer do nome do fichero *)
        nMaxFileTitle:=0; (* Tama�o do buffer lpstrFileTitle *)
        lpstrInitialDir:=nil; (* directorio inicial = actual *)
        lpstrTitle:=PCaption; (* t�tulo *)
        Flags := OFN_HIDEREADONLY;
        lpstrDefExt:=PExtension; (* extensi�n por defecto *)
        lCustData:=0;
        lpfnHook:=nil;
        lpTemplateName:='';
        lStructSize:= SizeOf(TOpenFilename)
      END; (* With AbrirFicheiro *)
  END; (* Procedure EncheAbrirFicheiro *)


(*########################################################################*)
(*             FUNCI�NS E PROCEDEMENTOS para DI�LOGOS                     *)
(*########################################################################*)
(*************************************************************************)
(*      Funci�n encargada do control do di�logo Datos_Rex:               *)
(*      Debe devolver: TRUE  Se procesa a mensaxe recibida               *)
(*                     FALSE Se non o procesa                            *)
(*************************************************************************)
FUNCTION ControlDialogoDatos_Rex(PDialogo        :HWnd;
                                 PMensaxe,PWParam:Word;
                                 PLParam         :Longint):Bool; export;
  VAR
    Nome  :array[0..40] of char;
    Numero:array[0..4]of char;
    Tonto :integer;
  BEGIN
    ControlDialogoDatos_Rex:=TRUE;
    CASE PMensaxe OF
      WM_INITDIALOG:
        BEGIN (* Inicializaci�n antes de visualizar o di�logo *)
          LStrCpy(@Nome,'Rexion');
          Str(Actual,Numero);
          LStrCat(@Nome,Numero);
          SendDlgItemMessage(PDialogo,CID_Nome,WM_SetText,0,longint(@Nome));
          SendDlgItemMessage(PDialogo,CID_Numero,WM_SetText,0,longint(@Numero));
        END;
      WM_COMMAND:
        CASE PWParam  OF
          ID_OK: (* gardamos o nome e ordinal da rexi�n e saimos do di�logo *)
            BEGIN
              SendDlgItemMessage(PDialogo,CID_Nome,WM_GetText,41,
                                 longint(@(A_Rexion[Actual].Inf_Rexion.Nome)));
              SendDlgItemMessage(PDialogo,CID_Numero,WM_GetText,5,longint(@Numero));
              val(Numero,A_Rexion[Actual].Inf_Rexion.Orden,tonto);
              EndDialog(PDialogo,PWParam)
            END; (* ID_OK *)
          ID_CANCEL: (* Cancelouse: saimos directamente do di�logo *)
            BEGIN
              EndDialog(PDialogo,PWParam)
            END
          ELSE (* Case PWParam *)
            ControlDialogoDatos_Rex:=FALSE (* Proceso por defecto *)
        END (* Case PWParam y WM_Command *)
      ELSE (* Case PMensaxe *)
        ControlDialogoDatos_Rex:=FALSE (* Proceso por defecto *)
    END (* Case PMensaxe *)
  END; (* Function ControlDialogoDatos_Rex *)

(*************************************************************************)
(*      Funci�n encargada do control do di�logo Sobre:                   *)
(*      Debe devolver: TRUE  Se procesa a mensaxe recibida               *)
(*                     FALSE Se non o procesa                            *)
(*************************************************************************)
FUNCTION ControlDialogoSobre(PDialogo        :HWnd;
                             PMensaxe,PWParam:Word;
                             PLParam         :Longint):Bool; export;
  BEGIN
    ControlDialogoSobre:=TRUE;
    CASE PMensaxe OF
      WM_INITDIALOG:
        BEGIN
          (* Inicializaci�n antes de visualizar o di�logo *)
        END;
      WM_COMMAND:
        CASE PWParam  OF
          ID_OK, ID_CANCEL:
            BEGIN
              EndDialog(PDialogo,PWParam)
            END
          ELSE (* Case PWParam *)
            ControlDialogoSobre:=FALSE (* Proceso por defecto *)
        END (* Case PWParam y WM_Command *)
      ELSE (* Case PMensaxe *)
        ControlDialogoSobre:=FALSE (* Proceso por defecto *)
    END (* Case PMensaxe *)
  END; (* Function ControlDialogoSobre *)

(*************************************************************************)
(*      Funci�n encargada do control da vent� principal                  *)
(*      Debe devolver: 0             Se procesa a Mensaxe recibida       *)
(*                     DefWindowProc Se non o procesa                    *)
(*************************************************************************)
FUNCTION ControlVP(PVP              :HWnd;
                   PMensaxe,PWParam :Word;
                   PLParam          :Longint):Longint; export;
  VAR
    Proceso : TFarProc;
  BEGIN
    ControlVP:=0; (* supo�emos que imos procesar a Mensaxe *)
    CASE PMensaxe OF
      WM_COMMAND: (* escolleuse unha opci�n do men� *)
        CASE PWParam OF
          ID_Novo: (* escolleuse a opci�n ID_Novo *)
            BEGIN
              IF Modificado THEN
              Resposta:=MessageBox(PVP,'O arquivo actual foi modificado:'+#13+
                                       '�quere gardalo antes de comezar o novo?','Aviso',
                                       MB_YESNOCANCEL OR MB_ICONSTOP);
              CASE Resposta OF
                ID_YES: SendMessage(PVP,WM_COMMAND,ID_Gardar,0);
                ID_CANCEL: Exit;
              END;
              (* Reseteamos a aplicaci�n *)
              Libera_A_Rexion;
              Inicia_A_Rexion;
              Inicia_Var
            END; (* ID_Novo *)
          ID_Abrir: (* escolleuse a opci�n ID_Abrir *)
            BEGIN
              EncheAbrirFicheiro(@NomeFicheiro[1],
                                 'Arquivos de rexi�ns'+#0+'*.r'+#0+'Todos os arquivos'+#0+'*.*'+#0+#0,
                                 'Abrir un arquivo de rexi�ns',
                                 'r');
              GetOpenFileName(AbrirFicheiro);
            END; (* ID_Abrir *)
          ID_Gardar: (* escolleuse a opci�n ID_Gardar *)
            BEGIN
              SendMessage(PVP,WM_COMMAND,ID_GardarC,0)
            END; (* ID_Gardar *)
          ID_GardarC: (* escolleuse a opci�n ID_GardarC *)
            BEGIN
              EncheAbrirFicheiro(@NomeFicheiro[1],
                                 'Arquivos de rexi�ns'+#0+'*.r'+#0+'Todos os arquivos'+#0+'*.*'+#0+#0,
                                 'Gardar un arquivo de rexi�ns',
                                 'r');
              GetSaveFileName(AbrirFicheiro);
            END; (* ID_GardarC *)
          ID_Sair: (* escolleuse a opci�n ID_Sair *)
            BEGIN
              PostMessage(VP,WM_CLOSE,0,0) (* tam�n se pode usar un SendMessage *)
            END; (* ID_Sair *)
          ID_Desfacer: (* escolleuse a opci�n ID_Desfacer *)
            BEGIN
            END; (* ID_Desfacer *)
          ID_Nova: (* escolleuse a opci�n ID_Nova *)
            BEGIN
              (* Facemos sitio para a posible nova rexi�n *)
              Inc(Actual);
              Crea_A_Rexion(Actual);
              (* Executamos o di�logo que enche autom�ticamente a nova rexi�n *)
              Proceso:=MakeProcInstance(@ControlDialogoDatos_Rex,
                                        EstaInstancia);
              Resposta:=DialogBox(EstaInstancia,'Datos_Rex',PVP,Proceso);
              FreeProcInstance(Proceso);
              (* Evaluamos a saida do di�logo *)
              IF Resposta=ID_Cancel
                THEN BEGIN
                       Destrue_A_Rexion(Actual);
                       Dec(Actual)
                     END
                ELSE Modificado:=TRUE
            END; (* ID_Nova *)
          ID_Eliminar: (* escolleuse a opci�n ID_Eliminar *)
            IF Actual<>0 THEN BEGIN
              Destrue_A_Rexion(Actual);
              Dec(Actual) (* seleccionamos a rexi�n anterior *)
            END; (* ID_Eliminar *)
          ID_Numerar: (* escolleuse a opci�n ID_Numerar *)
            BEGIN
            END; (* ID_Numerar *)
          ID_Fondo: (* escolleuse a opci�n ID_Fondo *)
            BEGIN
              (* Primeiro obtemos o ruta+nome do ficheiro bmp a cargar *)
              EncheAbrirFicheiro(@NomeFicheiro[1],
                                 'Arquivos de bitmap'+#0+'*.bmp'+#0+'Todos os arquivos'+#0+'*.*'+#0+#0,
                                 'Abrir un arquivo de bitmap',
                                 'bmp');
              IF GetOpenFileName(AbrirFicheiro) THEN
                BEGIN
                  (* agora cargamos o bitmap e o copiamos a vent� *)
                  OBitmap:=CargaFicheiroBMP(@NomeFicheiro[1]);
                  IF OBitmap=0 THEN
                    BEGIN
                      MessageBox(VP,'Erro durante a carga do arquivo bmp','Erro',
                                 MB_ICONEXCLAMATION OR MB_OK);
                      EXIT
                    END;
                  PintaOBitmap(VPDC,OBitmap); (* copiar *)
                END (* IF GetOpenFileName *)
            END; (* ID_Fondo *)
          ID_Contido: (* escolleuse a opci�n ID_Contido *)
            BEGIN
            END; (* ID_Contido *)
          ID_Sobre: (* escolleuse a opci�n ID_Sobre *)
            BEGIN
              Proceso:=MakeProcInstance(@ControlDialogoSobre,EstaInstancia);
              DialogBox(EstaInstancia,'Sobre',PVP,Proceso);
              FreeProcInstance(Proceso)
            END (* ID_Sobre *)
        END; (* Case PWParam y WM_COMMAND *)
      WM_LBUTTONDOWN: (* Se preme o bot�n esquerdo *)
        IF Actual<>0 THEN
          BEGIN WITH A_Rexion[Actual] DO BEGIN
            Estado:=Debuxando;
            Inc(Contador); (* n� de ptos *)
            IF Contador=1 THEN
              BEGIN
                (* Almacenamos o punto inicial *)
                Ult_Punto^.x:=LoWord(PLParam);
                Ult_Punto^.y:=HiWord(PLParam);
                (* E volvemos incrementar o contador *)
                Inc(Contador) (* Contador vale 2 *)
              END;
            Inc(Ult_Punto); (* movemos o punteiro � seguinte pto *)
            (* gardamos o punto actual *)
            Ult_Punto^.x:=LoWord(PLParam);
            Ult_Punto^.y:=HiWord(PLParam);
            PintarUltimaLinha(Ult_Punto,TRUE)
          END (* With A_Rexion[Actual] *)
          END; (* If actual<>0 y WM_LButtonDown *)
      WM_LBUTTONUP: (* Se solta o bot�n esquerdo *)
        IF Estado=Debuxando THEN BEGIN
          PintarUltimaLinha(A_Rexion[Actual].Ult_Punto,FALSE); (* Pintamos a nova li�a *)
          Estado:=Normal
        END;
      WM_MOUSEMOVE: (* Se move o rato na vent� *)
        IF Estado=Debuxando THEN WITH A_Rexion[Actual] DO BEGIN
          PintarUltimaLinha(Ult_Punto,TRUE); (* Borramos a li�a anterior *)
          Ult_Punto^.x:=LoWord(PLParam);
          Ult_Punto^.y:=HiWord(PLParam);
          PintarUltimaLinha(Ult_Punto,TRUE) (* Pintamos a nova li�a *)
        END; (* IF Estado=Debuxando e With A_Rexion[Actual] e WM_MouseMove *)
      WM_RBUTTONUP: (* Se solta o bot�n dereito *)
        WITH A_Rexion[Actual] DO BEGIN
          Inc(Ult_Punto); (* movemos o punteiro � seguinte pto *)
          (* gardamos o punto actual *)
          Ult_Punto^.x:=Dat_Rexion^.X;
          Ult_Punto^.y:=Dat_Rexion^.Y;
          PintarUltimaLinha(A_Rexion[Actual].Ult_Punto,FALSE); (* Pintamos a nova li�a *)
          Estado:=Normal;
          Mango:=CreatePolygonRgn(Dat_Rexion^,Contador,ALTERNATE);
          IF Mango=0 THEN MessageBeep(1);
          {FillRgn(VPDC,Mango,CreateSolidBrush($00FF00FF));
          PaintRgn(VPDC,Mango);}
          FrameRgn(VPDC,Mango,0,1,1)
        END; (* WM_RBUTTONUP *)
      WM_TIMER: (* aviso de que pasaron x milisegundos *)
        BEGIN
          Inc(Flash);
          CASE Flash OF
            7 : BEGIN
                  {Estado:=Flasheando;}
                  IF Actual>0 THEN
                    IF A_Rexion[Actual].Mango<>0 THEN
                      InvertRgn(VPDC,A_Rexion[Actual].Mango)
                END; (* 7 *)
            8 : BEGIN
                  Flash:=0;
                  IF Actual>0 THEN
                    IF A_Rexion[Actual].Mango<>0 THEN
                      InvertRgn(VPDC,A_Rexion[Actual].Mango);
                  {Estado:=Normal}
                END (* 8 *)
          END; (* Case Flash of *)
        END; (* WM_TIMER *)
      WM_PAINT: (* hai que redibuxar a pantalla da vent� *)
        BEGIN
          RepintarVP;
        END; (* WM_PAINT *)
      WM_CLOSE: (* pediuse pechar a aplicaci�n *)
        BEGIN
          IF Modificado
            THEN Resposta:=MessageBox(PVP,'O arquivo actual foi modificado:'+#13+
                                      '�quere gardalo antes de sair?','Aviso',
                                      MB_YESNOCANCEL OR MB_ICONSTOP)
            ELSE Resposta:=ID_NO;
          CASE Resposta OF
            ID_YES: SendMessage(PVP,WM_COMMAND,ID_Gardar,0);
            ID_CANCEL: Exit
          END; (* Case Resposta *)
          DestroyWindow(PVP)
        END; (* WM_CLOSE *)
      WM_DESTROY: (* se indica que se destruye la ventana principal *)
        BEGIN
          PostQuitMessage(0)
        END (* WM_DESTROY *)
      ELSE (* Mensaxe no procesado por nosotros -> proceso por defecto *)
        ControlVP:=DefWindowProc(PVP,PMensaxe,PWParam,PLParam)
    END (* Case PMensaxe *)
  END; (* Function ControlVP *)


(**************************************************************************)
(*                 P R O G R A M A  P R I N C I P A L                     *)
(**************************************************************************)
BEGIN (* PROGRAMA PRINCIPAL *)
  (* Renombramos as variables do sistema *)
  InstanciaPrevia:=HPrevInst; (* instancia anterior do programa *)
  EstaInstancia  :=HInstance; (* esta instancia (esta execuci�n) *)

  (* comprobamos que non se est� executando xa o noso programa *)
  IF InstanciaPrevia<>0 THEN
  (* xa se est� executando, polo tanto o activamos e saimos deste *)
    BEGIN
      ShowWindow(FindWindow(Clase_Nome,VP_Nome),SW_RESTORE);
      HALT(0)
    END;

  (* enchemos a estructura_clase cos par�metros adecuados *)
  WITH ClaseVP DO
    BEGIN
      style        :=CS_BYTEALIGNCLIENT (* OR CS_BYTEALIGNWINDOW *)
                     OR CS_CLASSDC; (* estilos da clase *)
      lpfnWndProc  :=@ControlVP; (* funci�n que controlar� as vent�s *)
      cbClsExtra   :=0;          (* bytes extra para a clase *)
      cbWndExtra   :=0;          (* bytes extra para a vent� *)
      hInstance    :=EstaInstancia;   (* instancia dona da clase *)
      hIcon        :=LoadIcon(EstaInstancia,'OIcono');(* Icono  *)
      hCursor      :=LoadCursor(0,IDC_ARROW);         (* Cursor *)
      hbrBackground:=COLOR_WINDOW+1; (* cor de fondo por defecto da vent� *)
      lpszMenuName :='OMenu';        (* Men� para as vent�s da clase *)
      lpszClassName:=Clase_Nome;     (* nome identificativo da clase *)
    END;

  (* rexistramos a clase para ver se � correcta e poder usala *)
  IF NOT RegisterClass(ClaseVP) THEN
    (* a clase non se puido rexistrar e polo tanto abortamos a execuci�n *)
    BEGIN
      MessageBox(GetFocus,'Non se puido rexistrar a clase de VP','Erro',
                 MB_IconStop);
      HALT(255)
    END;

  (* Antes de crear a vent� principal calculamos as medidas e a posici�n da mesma *)
  AnchoVP:=AnchoCl+GetSystemMetrics(SM_CXBORDER)*2;
  AltoVP :=AltoCl+GetSystemMetrics(SM_CYBORDER)*2+GetSystemMetrics(SM_CYCAPTION)+
                  GetSystemMetrics(SM_CYMENU);
  PosiX  :=(GetSystemMetrics(SM_CXSCREEN)-AnchoVP)DIV 2;
  PosiY  :=(GetSystemMetrics(SM_CYFULLSCREEN)+GetSystemMetrics(SM_CYMENU)+
            GetSystemMetrics(SM_CYBORDER)*2-AltoVP)DIV 2;

  (* Agora creamos a Vent� Principal empregando a clase rexistrada *)
  VP:=CreateWindow(
    Clase_Nome,             (* clase � que pertencer� a vent� *)
    VP_Nome,                (* t�tulo/nome da vent� *)
    WS_OVERLAPPED OR WS_CAPTION OR WS_SYSMENU OR
    WS_MINIMIZEBOX,         (* estilos da vent� *)
    PosiX,                  (* Posici�n X da vent� *)
    PosiY,                  (* Posici�n Y da vent� *)
    AnchoVP,                (* Ancho da vent� *)
    AltoVP,                 (* Alto da vent� *)
    0,                      (* Padre desta vent�. 0 � o escritorio *)
    0,                      (* Men�. Neste caso empregamos o da clase *)
    EstaInstancia,          (* A aplicaci�n dona da vent� *)
    nil);                   (* punteiro innecesario *)

  (* Comprobamos que a vent� te�a sido creada correctamente *)
  IF VP=0 THEN
    (* non se puido crear a vent�, e por tanto abortamos a execuci�n *)
    BEGIN
      MessageBox(GetFocus,'Non se puido crear a vent� VP','Erro',
                 MB_IconStop);
      HALT(255)
    END;

  (* Mostramos a vent� e a actualizamos no sistema *)
  ShowWindow(VP, CmdShow);
  UpdateWindow(VP);

  (* Inicializamos as variables necesarias *)
  VPDC:=GetDC(VP); (* O Device Context para VP. Non � necesario liberalo *)
  Inicia_A_Rexion; (* Coloca a nil todas as rexi�ns *)
  Inicia_Var; (* inicia variables do programa *)
  IF SetTimer(VP,1,10,nil)=0 THEN HALT(255);


  (***********************************************************************)
  (*   BUCLE PRINCIPAL de mensaxer�a e eventos                           *)
  (***********************************************************************)
  WHILE GetMessage(Mensaxe,0,0,0) DO
    BEGIN
      TranslateMessage(Mensaxe); (* traduce os c�digos das teclas *)
      DispatchMessage(Mensaxe)   (* despacha as Mensaxes � aplicaci�n *)
    END;

  (* Finalizaci�n do programa tras o peche do bucle de eventos *)
  KillTimer (VP,1); (* matamos o timer que instalaramos *)
  Libera_A_Rexion;
  Halt(Mensaxe.WParam) (* C�digo de saida en Mensaxe.WParam *)
END.
(*  F I N A L  de  F I C H E R O  *)
