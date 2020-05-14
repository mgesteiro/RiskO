(**************************************************************************)
(*                                                                        *)
(*  JUEGO RISK: Adaptación al formato windows de PC de la versión 'Mac'.  *)
(*  POR: Eduardo Joaquín y Miguel Gesteiro. Verano 1994.                  *)
(*                                                                        *)
(**************************************************************************)

PROGRAM Risko;
(*$R Risko*)  (* Incluir el resource de nombre risk *)
(*$R+ *)
USES
  WinTypes,WinProcs,WinDos,OMemory,ShellAPI;
CONST
  VERSION     = 'RiskO v0.0beta';
  NIF         = chr(8)+'Esta es una partida de '+VERSION+'.'+chr(26);
  MaxMosaicos = 15; (* Máximo nº de mosaicos *)
  MaxColor    = 15; (* Máximo nº de colores *)
  MaxDibu     = MaxColor+MaxMosaicos;
  C : Array [1..6] OF TPoint = ((x:1;y:4),(x:5;y:13),(x:14;y:20),(x:21;y:26),(x:27;y:38),(x:39;y:42));
  Paleta : Array [1..MaxColor] OF Longint=($00C0C0C0,  (* gris         *)
                                           $000000FF,  (* rojo         *)
                                           $0000FF00,  (* Verde        *)
                                           $0000FFFF,  (* amarillo     *)
                                           $00FF0000,  (* azul         *)
                                           $00FF00FF,  (* violeta      *)
                                           $00000000,  (* negro        *)
                                           $00FFFFFF,  (* blanco       *)
                                           $0000007F,  (* rojo osc.    *)
                                           $00007F00,  (* verde osc.   *)
                                           $00007F7F,  (* amarillo osc.*)
                                           $007F0000,  (* azul osc.    *)
                                           $007F007F,  (* violeta osc. *)
                                           $007F7F00,  (* mar osc.     *)
                                           $007F7F7F); (* gris osc.    *)
TYPE
  TCamino  = Array [0..10] OF byte; (* Para hallar la ruta mas corta *)
  TJugador = RECORD
                 Nombre    : string[23];
                 Color     : byte;  (* Nº del color del jugador *)
                 NCartas   : byte;  (* Numero de cartas *)
                 LaCarta   : Array [1..9] of byte;  (* las cartas del jugador *)
                 AvanceAut : boolean; (* Avance automático o manual *)
                 Lucha     : byte; (* 1->1 a 1, 2->ganar-perder, 3->menos x ejercitos *)
                 Minimo    : byte; (* Minimos ejércitos a dejar cuando lucha es 3 *)
                 NDados    : byte; (* Nº de dados a lanzar en los combates *)
                 CobrarCar : byte; (* Nº de vez que se cobra carta *)
                 Total     : integer; (* Todo los ejercitos que se han tenido *)
                 Victorias : integer;
                 Derrotas  : integer;
                 Modalidad : byte; (* Puede ser 'machoman','neutro','caos',tu o nojuega *)
                 Perdidos  : byte; (* Contador matriz ElPerdido *)
                 ElPerdido : Array [1..10] OF byte; (* Paises perdidos *)
                 FinBolsa  : byte;
                 BolsaPais : Array [1..42] OF byte;
                 Conts     : byte; (* nº de continentes que se tienen *)
                 ElCont    : Array [1..6]  OF byte; (* los continentes que tengo *)
                 Continente: byte
               END;
  TPais    = RECORD
                 Ejercitos : byte;
                 Fortif    : shortint; (* Número de ejércitos fortificados *)
                 Recibido  : boolean;
                 Amo       : byte
               END;
  TEstado  = (Ninguno,Circulando,Iniciando,Repartiendo,Atacando,Ganando,Fortificando);
  TJuego   = RECORD
                 DNI       : string[43];
                 Activo    : boolean;
                 Actual    : byte; (* Posicion de poles con el Jugador que tiene el turno *)
                 Jugador   : Array [1..6] of TJugador;
                 Pais      : Array [1..42] of TPais;
                 Modalidad : byte; (* puede ser: 5,5,5...; 4,5,6,7...; 4,5,7,9,13... *)
                 Poles     : Array [1..6] OF Byte; (* Guardar los jugadores con sus posiciones *)
                 Estado    : TEstado;
                 PAtaca    : byte; (* El pais que está atacando *)
                 PFortifica: byte; (* El pais que está fortificando *)
                 Batalla   : Array[1..2] OF byte; (* paises que se reparten soldados sobrantes del ataque *)
                 Pri,Ult   : shortint; (* posiciones de cuartel que se han escogido   *)
                 Cuartel   : Array [1..10] OF byte; (* Los soldados entre que escoger *)
                 NumSold   : byte; (* Nº de soldados *)
                 Seleccion : boolean; (* Si se está seleccionando o no *)
                 Invadio   : boolean; (* Indica si se invadió durante ataque *)
                 Final     : boolean; (* Indica si se finaliza el programa *)
                 (* Contadores para el programa principal *)
                 i,j       : integer;
                 (* Variables de los algoritmos ordenador-jugantes *)
                 PaisEscogido : byte;
                 Camino       : TCamino;
                 CaminoPos    : byte;
                 PAF          : Array[1..10] OF byte; (* Paises A Fortificar *)
                 PAFS         : byte (* Posicion final de los PAF *)
               END;
  TCartas  = RECORD
                 Amo       : byte;    (* El dueño de la carta *)
                 Tipo      : byte;    (* El tipo: cañon, soldado... *)
                 Usada     : boolean; (* Si la carta está usada *)
                 Boton     : HWnd     (* La caja de chequeo   *)
               END;
  TCarMarc = Array [1..9] OF RECORD
                                 LaCarta : byte;
                                 Tipo    : byte;
                               END; (* Las cartas que se pueden marcar *)
  TDatPais = RECORD
                 Nombre    : string[25];
                 Fronteras : byte;  (* Número de paises fronterizos a este *)
                 Frontera  : Array [1..6] of byte;  (* Los paises fronterizos *)
                 Centro    : TPoint; (* Centro del cuadrado *)
                 Grafo     : HBitmap (* El dibujillo de la carta *)
               END;
  TBrocha  = RECORD
                 Usada     : boolean;
                 RGB       : HBrush
               END;
  TLugares = Array [1..42,1..7] OF HRgn;
  TFichRgn = File OF integer;
  TFichero = File OF TJuego;
  TFichP   = File OF TDatPais;

VAR
  Instancia                       : integer;
  ClaseVentana                    : TWndClass;
  Ventana,Hecho,Fortificar,Carta  : HWnd;
  Mensaje                         : TMsg;
  MDC                             : HDC; (* Contexto gráfico principal *)
  Dibujo,Creditos,Dorso           : HBitmap;
  Tu,Macho,Caos,Neutral           : HBitmap;
  Soldado,Canon,Caballo           : HBitmap;
  Uno,Cinco,Diez,Veinte,L         : HBitmap;
  Fuente,FuenteGrande             : HFont;
  OK,NO                           : HCursor;
  Brocha                          : Array[1..MaxDibu] OF TBrocha;
  Borde,Negro,Azul                : HBrush;
  NomFich,Directorio              : string[128];
  Juego                           : TJuego;
  Cartas                          : Array[1..48] OF TCartas;
  DatPais                         : Array[1..42] OF TDatPais;
  Lugares                         : TLugares;
  Aleatorio                       : boolean;
  Fichero                         : TFichero;
  (* Variables de salvaguarda provisional *)
  OldColor                        : Array[1..6] OF byte;
  A1,A2,A3                        : byte;
  A4                              : boolean;
  HuboCambio                      : boolean;
  (* Variables para el cuadro de selección *)
  Inicio,Fin                      : TPoint;
  (* Contadores inútiles y variable que no hay que recuperar *)
  AuxS                            : string;
  PA,PD                           : byte; (* Pais Ataca, Pais Defiende *)
  m,pos,NFAF                      : byte;
  Suma                            : integer;

(************************************************************************************)
(************************************************************************************)
FUNCTION Elev(PBase:Real;exp:byte):Real;
  BEGIN
    IF exp=1
      THEN Elev:=PBase
      ELSE Elev:=Elev(PBase,Exp-1)*PBase
  END; (* Elev *)
(************************************************************************************)
(************************************************************************************)
PROCEDURE IniDos;
  VAR
    pm,pj : byte;
  BEGIN
    WITH Juego DO
      BEGIN
        DNI    := NIF;
        Activo := False; (* Indica si se está jugando o no una partida *)
        Actual := 1;  (* Indica la posición de poles del jugador que tiene el turno *)
        (* Inicialización del record jugador *)
        FOR pm:=1 TO 6 DO WITH Jugador[pm] DO
          BEGIN
            Color     :=pm;  (* Colores de salida *)
            NCartas   :=0;
            FOR pj:=1 TO 9 DO LaCarta[pj]:=0;
            AvanceAut :=True;
            Lucha     :=3; (* gane o pierda excepto x ejercitos *)
            Minimo    :=2; (* 2 ejércitos minimo *)
            NDados    :=3;
            CobrarCar :=1; (* Nº de vez que se cobra carta *)
            Total     :=0; (* Todos los ejércitos que se han tenido *)
            Victorias :=0;
            Derrotas  :=0;
            Modalidad :=5; (* En principio nadie Juega *)
            FinBolsa  :=0;
            FOR pj:=1 TO 42 DO BolsaPais[pj]:=0;
            Perdidos  :=0;
            FOR pj:=1 TO 10 DO ElPerdido[pj]:=0;
            Continente:=0;
            Conts     :=0;
            FOR pj:=1 TO 6  DO ElCont[pj]:=0
          END; (* WITH Jugador[pm]
        (* Inicialización del record pais *)
        FOR pm:=1 TO 42 DO WITH Pais[pm] DO
          BEGIN
            Ejercitos :=0;
            Fortif    :=0;
            Recibido  :=False;
            Amo       :=0
          END; (* WITH Pais[pm] *)
        Modalidad     :=1; (* Cartas: 5,5,5,... *)
        (* Inicializar poles *)
        FOR pm:=1 TO 6  DO Poles[pm]:=0;
        Estado:=Ninguno;
        PAtaca:=0; (* Pais que está atacando *)
        PFortifica:=0; (* Pais que está fortificando *)
        Invadio:=False;(* Indica si se invadió durante un ataque *)
        PaisEscogido:=0;
        FOR pm:=0 TO 10 DO Camino[pm]:=0;
        CaminoPos:=0
      END; (* WITH Pjuego *)
    (* Inicialización del record Cartas *)
    (* Tipo de cartas: 1->Soldado 2->Cañón 3->Caballo 4->Comodín *)
    FOR pm:=1 TO 48 DO WITH Cartas[pm] DO
      BEGIN
        Amo:=0;
        IF pm>42 THEN Tipo:=4
                 ELSE
                   BEGIN
                     pj:=pm; WHILE pj>3 DO pj:=pj-3; Tipo:=pj
                   END;
        Usada:=False
      END; (* For de cartas *)
    (* Inicialización de las brochas usadas que son colores por defecto *)
    FOR pj:=1 TO 6 DO Brocha[pj].Usada:=True;
    FOR pj:=7 TO MaxDibu DO Brocha[pj].Usada:=False;
    (* Inicializacion de variables no juego ni cartas *)
    NomFich:='';Directorio:='';
    Aleatorio:=True;
    FOR pm:=1 TO 10 DO Juego.Cuartel[pm]:=0;
    Juego.Ult:=0;Juego.Pri:=0;
    Juego.NumSold:=0;
    Juego.Batalla[1]:=0;Juego.Batalla[2]:=0;
    Juego.Seleccion:=False;
    FOR pm:=1 TO 6 DO OldColor[pm]:=0;
    A1:=0;A2:=0;A3:=0;A4:=False;
    HuboCambio:=False
  END; (* IniDos *)
(********************************************************************************)
(********************************************************************************)
PROCEDURE Ini;
  VAR
    pj,pm,pt       : integer;
    FichRgn        : TFichRgn;
    FichP          : TFichP;
    NumPol,NumPtos : integer;
    Ptr            : pointer;
    PtrPunto       : ^TPoint;
    Punto          : TPoint;
  BEGIN
    Randomize;
    (* Cargar las regiones de los paises *)
    FOR pj:=1 TO 42 DO FOR pm:=1 TO 7 DO Lugares[pj,pm]:=0;
    Assign(FichRgn,'Risko.001');
    Reset(FichRgn);
    FOR pj:=1 TO 42 DO
      BEGIN
        Read(FichRgn,NumPol);
        FOR pm:=1 TO NumPol DO
          BEGIN
            Read(FichRgn,NumPtos);
            Ptr:=MemAlloc(SizeOf(TPoint)*NumPtos);
            PtrPunto:=Ptr;
            FOR pt:=1 TO NumPtos DO
              BEGIN
                Read(FichRgn,Punto.X);
                Read(FichRgn,Punto.Y);
                PtrPunto^:=Punto;
                Inc(PtrPunto)
              END;
            Lugares[pj,pm]:=CreatePolygonRgn(Ptr^,NumPtos,Alternate);
            FreeMem(Ptr,SizeOf(TPoint)*NumPtos)
          END (* For NumPol *)
      END; (* For pj *)
    Close(FichRgn);
    (* Cargar los datos de los paises *)
    Assign(FichP,'Risko.002');
    Reset(FichP);
    FOR pj:=1 TO 42 DO Read(FichP,DatPais[pj]);
    Close(FichP);
    FOR pj:=1 TO 42 DO DatPais[pj].Grafo:=LoadBitmap(Instancia,MakeIntResource(pj));
    (* Crear las brochas que serán colores y 'patterns' *)
    FOR pj:=1 TO MaxColor DO Brocha[pj].RGB:=CreateSolidBrush(Paleta[pj]);
    FOR pj:=MaxColor+1 TO MaxDibu DO
      Brocha[pj].RGB:=CreatePatternBrush(LoadBitmap(Instancia,MakeIntResource(pj-MaxColor+50)));
    Borde:=CreatePatternBrush(LoadBitmap(Instancia,'Aduana'));
    Negro:=CreateSolidBrush($00000000);
    Azul :=CreateSolidBrush($00FFFF00);
    (* Carga de la fuente de tamaño 11 para el programa *)
    Fuente:=CreateFont(11,0,0,0,400,0,0,0,0,OUT_Default_Precis,CLIP_Default_Precis,
                       Default_Quality,Default_Pitch OR FF_DontCare,'Arial');
    FuenteGrande:=CreateFont(13,9,0,0,700,0,0,0,0,OUT_Default_Precis,CLIP_Default_Precis,
                             Default_Quality,Default_Pitch OR FF_DontCare,'Courier');
    (* Carga de los tipos de soldados *)
    Dibujo   :=LoadBitmap(Instancia,'MapaMundo');
    Creditos :=LoadBitmap(Instancia,'Creditos');
    Dorso    :=LoadBitmap(Instancia,'Dorso');
    Uno      :=LoadBitmap(Instancia,{'I'} 'Uno');
    Cinco    :=LoadBitmap(Instancia,{'V'} 'Cinco');
    Diez     :=LoadBitmap(Instancia,{'X'} 'Diez');
    Veinte   :=LoadBitmap(Instancia,{'XX'}'Veinte');
    L        :=LoadBitmap(Instancia,{'L'} 'Cincuenta');
    Tu       :=LoadBitmap(Instancia,'Gicho');
    Macho    :=LoadBitmap(Instancia,'Machoman');
    Caos     :=LoadBitmap(Instancia,'Caotico');
    Neutral  :=LoadBitmap(Instancia,'Neutral');
    Soldado  :=LoadBitmap(Instancia,'Soldado');
    Caballo  :=LoadBitmap(Instancia,'Caballo');
    Canon    :=LoadBitmap(Instancia,'Cañon');
    (* Carga de los cursores del dragging *)
    OK       :=LoadCursor(Instancia,'RiskOk');
    NO       :=LoadCursor(Instancia,'RiskNo');
    (* Inicialización del record juego,cartas y otras variables *)
    IniDos
  END; (* Procedure IniPais *)
(**************************************************************************************)
(**************************************************************************************)
 PROCEDURE PintarPais(PNumPais:byte;PBorde:HBrush);
  VAR
    pj     : integer;(* Contador *)
    AuxNum : byte;
    Aux    : string;
  BEGIN (* PintarPais *)
    IF ( (PNumPais<>0)AND(Juego.Pais[PNumPais].Amo<>0) )
      THEN
        BEGIN
          IF Juego.Pais[PNumPais].Fortif>0
            THEN AuxNum:=Juego.Pais[PNumPais].Ejercitos+Juego.Pais[PNumPais].Fortif
            ELSE AuxNum:=Juego.Pais[PNumPais].Ejercitos;
          IF AuxNum<>0
            THEN
              BEGIN
                FOR pj:=1 TO 7 DO
                  IF Lugares[PNumPais,pj]<>0
                    THEN
                      BEGIN
                        FillRgn(MDC,Lugares[PNumPais,pj],Brocha[Juego.Jugador[Juego.Pais[PNumPais].Amo].Color].RGB);
                        FrameRgn(MDC,Lugares[PNumPais,pj],PBorde,1,1)
                      END;
                Rectangle(MDC,DatPais[PNumPais].Centro.x-6,DatPais[PNumPais].Centro.y-6,
                              DatPais[PNumPais].Centro.x+7,DatPais[PNumPais].Centro.y+7);
                Str(AuxNum,Aux);
                TextOut(MDC,DatPais[PNumPais].Centro.x,DatPais[PNumPais].Centro.y-5,@Aux[1],Length(Aux))
              END
            ELSE
              BEGIN  (* AuxNum=0 *)
                SelectObject(MDC,Azul); SelectObject(MDC,GetStockObject(Null_Pen));
                Rectangle(MDC,DatPais[PNumPais].Centro.x-6,DatPais[PNumPais].Centro.y-6,
                              DatPais[PNumPais].Centro.x+8,DatPais[PNumPais].Centro.y+8);
                SelectObject(MDC,GetStockObject(White_Brush));SelectObject(MDC,GetStockObject(Black_Pen));
                FOR pj:=1 TO 7 DO
                  IF Lugares[PNumPais,pj]<>0
                    THEN
                      BEGIN
                        FillRgn(MDC,Lugares[PNumPais,pj],Brocha[Juego.Jugador[Juego.Pais[PNumPais].Amo].Color].RGB);
                        FrameRgn(MDC,Lugares[PNumPais,pj],PBorde,1,1)
                      END
              END  (* AuxNum=0 *)
        END (* If pais y amo<>0 *)
  END; (* PintarPais *)
(**************************************************************************************)
PROCEDURE Repintar(PDC:HDC); Forward;
(**************************************************************************************)
PROCEDURE RepartirTerritorios;
  VAR
    t,pj      : integer;
    Semilla   : byte;
    OldCursor : HCursor;
  BEGIN
    (* colocamos el reloj de espera mientras repartimos paises *)
    OldCursor:=SetCursor(LoadCursor(0,IDC_Wait));
    pj:=1;
    FOR t:=1 TO 42 DO
      BEGIN
        WHILE Juego.Jugador[pj].Modalidad=5 DO IF pj=6
          THEN Pj:=1
          ELSE Pj:=Pj+1;
        REPEAT Semilla:=Random(42)+1 UNTIL (Juego.Pais[Semilla].Amo=0);
        Juego.Pais[Semilla].Amo:=pj;
        PintarPais(Semilla,Negro);
        IF pj=6 THEN pj:=1
                ELSE pj:=Pj+1
      END; (* for *)
    SetCursor(OldCursor)
  END; (* RepartirTerritorios *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE CambiarColor(PJu: byte; x1,y1,x2,y2:integer; PIncremento:shortint; PDC: HDC);
  VAR
    pj          : byte;
    BrochaVieja : HBrush;
  BEGIN
    (* Avisar de que se ha producido un cambio *)
    HuboCambio:=True;
    (* Cambio del color propiamente dicho *)
    pj:=Juego.Jugador[PJu].Color;
    REPEAT
      pj:=pj+PIncremento;
      IF pj>MaxDibu THEN pj:=1;
      IF pj<1 THEN pj:=MaxDibu
    UNTIL (NOT Brocha[pj].Usada);
    Brocha[Juego.Jugador[PJu].Color].Usada:=False;
    Juego.Jugador[PJu].Color:=pj;
    Brocha[pj].Usada:=True;
    BrochaVieja:=SelectObject(PDC,Brocha[pj].RGB);
    Rectangle(PDC,x1,y1,x2,y2);
    Brocha[pj].RGB:=SelectObject(PDC,BrochaVieja)
  END; (* CambiarColor *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION TotalCobrar(PJug: byte):integer;
  VAR
    c,NumPaises : byte;
    Aux,TotSold : integer;
  FUNCTION Continente(uno,dos:byte):boolean;
    VAR
      t : byte;
    BEGIN
      Continente:=True;
      FOR t:=uno TO dos DO IF Juego.Pais[t].Amo<>PJug THEN Continente:=False
    END; (* Continente *)
  BEGIN
    Aux:=3;NumPaises:=0;TotSold:=0;
    IF Continente( 1, 4) THEN Aux:=Aux+2;
    IF Continente( 5,13) THEN Aux:=Aux+5;
    IF Continente(14,20) THEN Aux:=Aux+5;
    IF Continente(21,26) THEN Aux:=Aux+3;
    IF Continente(27,38) THEN Aux:=Aux+7;
    IF Continente(39,42) THEN Aux:=Aux+2;
    FOR c:=1 TO 42 DO IF Juego.Pais[c].Amo=PJug THEN Inc(NumPaises);
    IF NumPaises>11 THEN Inc(Aux,((NumPaises-9)DIV 3));
    FOR c:=1 TO 42 DO IF Juego.Pais[c].Amo=PJug THEN Inc(TotSold,Juego.Pais[c].Ejercitos);
    IF TotSold+Aux>NumPaises*99 then Aux:=(99*NumPaises)-TotSold;
    TotalCobrar:=Aux
  END; (* TotalCobrar *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE DibujarTurnos;
  VAR
    NumJug      : byte;
    Pj,pz       : byte;
    BrochaVieja : HBrush;
    Aux         : string;
  BEGIN
    (* Vaciar el recuadro *)
    Rectangle(MDC,6,218,55,435);
    (* Comienzo *)
    NumJug:=0;
    FOR Pj:=1 TO 6 DO IF Juego.Poles[Pj]<>0 THEN Inc(NumJug);
    pz:=Juego.Actual;
    (* Escribir el nombre del jugador que tiene el turno *)
    SelectObject(MDC,FuenteGrande); SetTextAlign(MDC,TA_Left OR TA_Top);
    TextOut(MDC,10,10,'                       ',23);
    TextOut(MDC,10,10,@Juego.Jugador[Juego.Poles[pz]].Nombre[1],Length(Juego.Jugador[Juego.Poles[pz]].Nombre));
    SelectObject(MDC,Fuente); SetTextAlign(MDC,TA_Center OR TA_Top);
    (* Pintar los recuadros y el numero de cartas *)
    FOR Pj:=1 TO NumJug DO
      BEGIN
        BrochaVieja:=SelectObject(MDC,Brocha[Juego.Jugador[Juego.Poles[pz]].Color].RGB);
        Rectangle(MDC,9,221+(Pj-1)*(214 DIV NumJug),45,219+Pj*(214 DIV NumJug));
        Str(Juego.Jugador[Juego.Poles[pz]].NCartas,Aux);
        TextOut(MDC,48,222+(Pj-1)*(214 DIV NumJug),'   ',3);
        TextOut(MDC,48,222+(Pj-1)*(214 DIV NumJug),@Aux[1],1);
        Brocha[Juego.Jugador[Juego.Poles[pz]].Color].RGB:=SelectObject(MDC,BrochaVieja);
        IF pz=NumJug THEN pz:=1
                     ELSE pz:=pz+1
      END
  END; (* DibujarTurnos *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE DibujarCarta(PDC:HDC; X,Y:integer; Pj:byte);
  VAR
    MemDC : HDC;
  BEGIN
    SetTextAlign(PDC,TA_Center OR TA_Top);
    SelectObject(PDC,Fuente);
    RoundRect(PDC,x,y,x+102,y+152,15,9);
    MemDC:=CreateCompatibleDC(PDC);
    IF Cartas[Pj].Tipo=4 THEN
      BEGIN
        SelectObject(MemDC,Soldado);
        BitBlt(PDC,x+19,y+3,64,54,MemDC,0,0,SRCCopy);
        SelectObject(MemDC,Canon);
        BitBlt(PDC,x+19,y+57,64,36,MemDC,0,0,SRCCopy);
        SelectObject(MemDC,Caballo);
        BitBlt(PDC,x+19,y+93,64,54,MemDC,0,0,SRCCopy)
      END
      ELSE
        BEGIN
          TextOut(PDC,x+50,y+2,@DatPais[Pj].Nombre[1],Length(DatPais[Pj].Nombre));
          SelectObject(MemDC,DatPais[Pj].Grafo);
          BitBlt(PDC,x+5,y+14,92,77,MemDC,0,0,SRCCopy);
          CASE Cartas[Pj].Tipo OF
            1: BEGIN
                 SelectObject(MemDC,Soldado);
                 BitBlt(PDC,x+19,y+93,64,54,MemDC,0,0,SRCCopy)
               END;
            2: BEGIN
                 SelectObject(MemDC,Canon);
                 BitBlt(PDC,x+19,y+102,64,36,MemDC,0,0,SRCCopy)
               END;
            3: BEGIN
                 SelectObject(MemDC,Caballo);
                 BitBlt(PDC,x+19,y+93,64,54,MemDC,0,0,SRCCopy)
               END
          END (* Case *)
        END; (* Else *)
    DeleteDC(MemDC)
  END; (* DibujarCarta *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION TotalSoldados(PPri,PUlt:shortint):byte;
  VAR
    pm,Aux : byte;
  BEGIN
    Aux:=0;
    IF PPri>0     THEN FOR pm:=PPri TO PUlt DO Aux:=Aux+Juego.Cuartel[pm];
    IF PPri<>PUlt THEN FOR pm:=PUlt TO PPri DO Aux:=Aux+Juego.Cuartel[pm];
    Juego.Pri:=0; Juego.Ult:=0;
    IF Aux=0 THEN Aux:=1;
    IF Mensaje.WParam=MK_Shift THEN Aux:=Juego.NumSold;
    TotalSoldados:=Aux
  END; (* TotalSoldados *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE IndicarSoldados(PPri,PUlt:shortint);
(* Invierte los soldados que se han escogido y dice cuantos suman *)
  VAR
    MemDC : HDC;
    pm    : byte;
  BEGIN
    IF ((PPri>0)AND(PUlt>0)AND(Juego.NumSold>0)) THEN
      BEGIN
        MemDC:=CreateCompatibleDC(MDC);
        SelectObject(MemDC,Uno);
        FOR Pm:=(PPri-1) TO (PUlt-1) DO IF Juego.Cuartel[pm+1]<>0 THEN
          BitBlt(MDC,214+42*Pm,383,34,47,MemDC,0,0,DstInvert);
        IF PPri<>PUlt THEN
          FOR Pm:=(PUlt-1) TO (PPri-1) DO IF Juego.Cuartel[pm+1]<>0 THEN
            BitBlt(MDC,214+42*Pm,383,34,47,MemDC,0,0,DstInvert);
        DeleteObject(MemDC)
      END
  END; (* IndicarSoldados *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE PintarMonecos;
  VAR
    MemDC       : HDC;
    Posicion,pm : byte;
  BEGIN
    IF Juego.NumSold>0 THEN
      BEGIN
        Rectangle(MDC,210,378,631,434); (* Borrar los creditos *)
        (* Pintado de los muñecos *)
        MemDC:=CreateCompatibleDC(MDC); Posicion:=0;
        FOR pm:=1 TO 10 DO IF Juego.Cuartel[pm]=0 THEN Posicion:=pm;
        FOR pm:=10 DOWNTO Posicion+1 DO
          BEGIN
            CASE Juego.Cuartel[pm] OF
              1  : SelectObject(MemDC,Uno);
              5  : SelectObject(MemDC,Cinco);
              10 : SelectObject(MemDC,Diez);
              20 : SelectObject(MemDC,Veinte);
              50 : SelectObject(MemDC,L)
            END; (* Case *)
            BitBlt(MDC,214+42*(pm-1),383,34,47,MemDC,0,0,SRCCopy)
          END; (* FOR *)
        DeleteDC(MemDC)
      END (* IF NumSold>0 *)
  END; (* PintarMonecos *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE PintarSoldados(PNumero:integer);
  VAR
    Dividendo : byte;
    Posicion  : byte;
    pm        : byte; (* Contador *)
    Estat     : Array [1..5] OF byte;
    Acabouse  : boolean;

  FUNCTION SumaEstat:byte;
    BEGIN
      SumaEstat:=Estat[1]+Estat[2]+Estat[3]+Estat[4]+Estat[5]
    END;

  BEGIN (* PintarSoldados *)
    (* Inicialización de las posiciones para el Juego.Cuartel *)
    Juego.Pri:=0; Juego.Ult:=0;
    FOR pm:=1 TO 10 DO Juego.Cuartel[pm]:=0; (* Vaciamos el cuartel *)
    (* relleno de las estat *)
    Dividendo:=PNumero;
    Estat[3]:=Dividendo DIV 50; Dec(Dividendo,Estat[3]*50);
    Estat[1]:=Dividendo DIV 20; Dec(Dividendo,Estat[1]*20);
    Estat[2]:=Dividendo DIV 10; Dec(Dividendo,Estat[2]*10);
    Estat[4]:=Dividendo DIV 5;  Dec(Dividendo,Estat[4]*5);
    Estat[5]:=Dividendo; Dividendo:=0;
    (* Expansión de las estat *)
    Acabouse:=False;
    Posicion:=4;
    WHILE NOT Acabouse DO
      BEGIN
        IF SumaEstat<>10
          THEN WHILE ( (Posicion>0)AND(Estat[Posicion]=0) ) DO Dec(Posicion)
          ELSE Acabouse:=True;
        CASE Posicion OF
          0 : Acabouse:=True;
          3 : (* L *)
              IF SumaEstat+2<11
                THEN
                  BEGIN
                    Posicion:=4;
                    Inc(Estat[1],2);
                    Inc(Estat[2],1);
                    Dec(Estat[3],1)
                  END
                ELSE Dec(Posicion);
          1 : (* XX *)
              IF SumaEstat+1<11
                THEN
                  BEGIN
                    Posicion:=4;
                    Inc(Estat[2],2);
                    Dec(Estat[1],1)
                  END
                ELSE Dec(Posicion);
          2 : (* X *)
              IF SumaEstat+1<11
                THEN
                  BEGIN
                    Posicion:=4;
                    Inc(Estat[4],2);
                    Dec(Estat[2],1)
                  END
                ELSE Dec(Posicion);
          4 : (* V *)
              IF SumaEstat+4<11
                THEN
                  BEGIN
                    Posicion:=4;
                    Inc(Estat[5],5);
                    Dec(Estat[4],1)
                  END
                ELSE Dec(Posicion)
        END;  (* Case *)
    END; (* While not fin *)
    (* Rellenamos cuartel con las estat *)
    Posicion:=10;
    FOR pm:=1 TO Estat[3] DO
      BEGIN
        Juego.Cuartel[Posicion]:=50;
        Dec(Posicion)
      END;
    FOR pm:=1 TO Estat[1] DO
      BEGIN
        Juego.Cuartel[Posicion]:=20;
        Dec(Posicion)
      END;
    FOR pm:=1 TO Estat[2] DO
      BEGIN
        Juego.Cuartel[Posicion]:=10;
        Dec(Posicion)
      END;
    FOR pm:=1 TO Estat[4] DO
      BEGIN
        Juego.Cuartel[Posicion]:=5;
        Dec(Posicion)
      END;
    FOR pm:=1 TO Estat[5] DO
      BEGIN
        Juego.Cuartel[Posicion]:=1;
        Dec(Posicion)
      END;
    (* Pintar los soldados en pantalla *)
    PintarMonecos
  END; (* PintarSoldados *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE Repintar(PDC :HDC);
  VAR
    MemDC : HDC;
    pl    : byte;
  BEGIN
    (* Eliminación del aviso de cambio *)
    HuboCambio:=False;
    (* Reponer Dibujos *)
    MemDC:=CreateCompatibleDC(PDC);
    SelectObject(MemDC, Dibujo);
    BitBlt(MDC,0,0,638,440,MemDC,0,0,SRCCopy);
    SelectObject(MemDC, Creditos);
    BitBlt(MDC,211,379,420,55,MemDC,0,0,SRCCopy);
    DeleteDC(MemDC);
    (* Reponer paises *)
    FOR pl:=1 TO 42 DO PintarPais(pl,Negro);
    (* Reponer segun caso *)
    IF Juego.Estado<>Ninguno THEN
      BEGIN
        DibujarTurnos;
        CASE Juego.Estado OF
          Iniciando,Repartiendo,Ganando,Fortificando:
            IF Juego.NumSold>0 THEN
              BEGIN
                PintarSoldados(Juego.NumSold);
                IndicarSoldados(Juego.Pri,Juego.Ult);
                IF Juego.Estado IN [Ganando,Fortificando] THEN PintarPais(Juego.PAtaca,Borde);
              END;
          Atacando:
            IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Borde)
        END (* Case *)
      END; (* IF juego.estado<>Ninguno *)
    (* Enseñar los botones *)
    ShowWindow(Hecho,SW_ShowNA);
    ShowWindow(Fortificar,SW_ShowNA);
    ShowWindow(Carta,SW_ShowNA)
  END; (* Repintar *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE MoverTurno;
  BEGIN
    IF Juego.Actual<>6
      THEN IF Juego.Poles[Juego.Actual+1]<>0
        THEN Inc(Juego.Actual)
        ELSE Juego.Actual:=1
      ELSE Juego.Actual:=1
  END; (* MoverTurno *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION QuePais(Px,Py:integer):Byte;
  VAR
    pj,pm : byte;
  BEGIN
    QuePais:=0;
    FOR pj:=1 TO 42 DO FOR pm:=1 TO 7 DO
      IF PtInRegion(Lugares[pj,pm],Px,Py) THEN QuePais:=pj
  END; (* QuePais *)
(**************************************************************************************)








(**************************************************************************************)
(* Función que maneja la pantalla de victoria *)
FUNCTION Victoria(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    DCLocal,MemDC  : HDC;
    Planeta        : HBitmap;
  BEGIN
    Victoria := True; (* Por defecto se supone que se procesa *)
    CASE PMensaje OF
      WM_initdialog:
        BEGIN
          (* Colocar el dibujo *)
          Planeta:=LoadBitmap(Instancia,'PantallaVictoria');
          DCLocal := GetDC(PDialogo);
          MemDC := CreateCompatibleDC(DCLocal);
          ShowWindow(PDialogo,SW_ShowNormal);
          SelectObject(MemDC,Planeta);
          BitBlt(DCLocal,0,0,350,300,MemDC,0,0,SRCCopy);
          (* Liberación del contexto grafico *)
          DeleteDC(MemDC);
          ReleaseDC(PDialogo,DCLocal);
          DeleteObject(Planeta)
        END; (* WM_InitDialog *)
      WM_Command:
          IF PWParam=ID_OK
            THEN EndDialog(PDialogo,1)
            ELSE Victoria := False
      ELSE Victoria := False
    END (* Case PMensaje of *)
  END; (* Victoria *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION EnemigosQueSobreviven(Atacante,Defensor:byte):shortint;
  VAR
    EjerA,EjerD : shortint;
  FUNCTION Dificultad:byte;
    FUNCTION Pareja(A,B:byte):boolean;
      BEGIN
        IF (((Atacante=A)AND(Defensor=B))OR((Atacante=B)AND(Defensor=A)))
          THEN Pareja:=True
          ELSE Pareja:=False
      END; (* Pareja *)
    BEGIN  (* Dificultad *)
      IF        Pareja(37,11) THEN Dificultad:=2
        ELSE IF Pareja( 3,21) THEN Dificultad:=2
        ELSE IF Pareja(13,14) THEN Dificultad:=2
        ELSE IF Pareja(29,39) THEN Dificultad:=2
        ELSE IF Pareja( 4, 5) THEN Dificultad:=1
        ELSE IF Pareja( 8,13) THEN Dificultad:=1
        ELSE IF Pareja(14,15) THEN Dificultad:=1
        ELSE IF Pareja(14,16) THEN Dificultad:=1
        ELSE IF Pareja(15,20) THEN Dificultad:=1
        ELSE IF Pareja(15,18) THEN Dificultad:=1
        ELSE IF Pareja(15,16) THEN Dificultad:=1
        ELSE IF Pareja(16,18) THEN Dificultad:=1
        ELSE IF Pareja(20,21) THEN Dificultad:=1
        ELSE IF Pareja(19,21) THEN Dificultad:=1
        ELSE IF Pareja(19,22) THEN Dificultad:=1
        ELSE IF Pareja(27,23) THEN Dificultad:=1
        ELSE IF Pareja(23,24) THEN Dificultad:=1
        ELSE IF Pareja(25,24) THEN Dificultad:=1
        ELSE IF Pareja(34,38) THEN Dificultad:=1
        ELSE IF Pareja(37,38) THEN Dificultad:=1
        ELSE IF Pareja(39,40) THEN Dificultad:=1
        ELSE IF Pareja(39,41) THEN Dificultad:=1
        ELSE IF Pareja(40,41) THEN Dificultad:=1
        ELSE IF Pareja(40,42) THEN Dificultad:=1
        ELSE Dificultad:=0
    END; (* Dificultad *)
  FUNCTION TirarDados:boolean;
    VAR
      pj,TiradaA,TiradaD : byte;
      NumTiradas         : byte;
    BEGIN  (* TirarDados *)
      NumTiradas:=Juego.Jugador[Juego.Poles[Juego.Actual]].NDados;
      IF Juego.Pais[Atacante].Ejercitos<Juego.Jugador[Juego.Poles[Juego.Actual]].NDados THEN
        NumTiradas:=Juego.Pais[Atacante].Ejercitos;
      REPEAT
        TiradaA:=0;TiradaD:=0;
        FOR pj:=1 TO NumTiradas DO
          BEGIN Inc(TiradaA,Random(6-Dificultad)+1);Inc(TiradaD,Random(6)+1) END
      UNTIL TiradaA<>TiradaD;
      IF TiradaA>TiradaD THEN TirarDados:=True
                         ELSE TirarDados:=False
    END; (* TirarDados *)

  BEGIN (* EnemigosQueSobreviven *)
    EjerA:=Juego.Pais[Atacante].Ejercitos;
    EjerD:=Juego.Pais[Defensor].Ejercitos;
    CASE Juego.Jugador[Juego.Pais[Atacante].Amo].Lucha OF
      1 : (* Uno cada Vez *)
          BEGIN
            REPEAT
              IF TirarDados THEN Dec(EjerD)
                            ELSE Dec(EjerA)
            UNTIL ((EjerA<Juego.Pais[Atacante].Ejercitos)OR(EjerD<0))
          END; (* 1 *)
      2 : (* Hasta que gane o pierda *)
          BEGIN
            REPEAT
              IF TirarDados THEN Dec(EjerD)
                            ELSE Dec(EjerA)
            UNTIL ((EjerA=0)OR(EjerD<0))
          END;
      3 : (* Hasta que gane o pierda dejando MINIMO ejércitos *)
          BEGIN
            REPEAT
              IF TirarDados THEN Dec(EjerD)
                            ELSE Dec(EjerA)
            UNTIL ((EjerA<=Juego.Jugador[Juego.Pais[Atacante].Amo].Minimo)OR(EjerD<0))
          END
    END; (* Case *)
    Juego.Pais[Atacante].Ejercitos:=EjerA;
    IF EjerD<0 THEN Juego.Pais[Defensor].Ejercitos:=0
               ELSE Juego.Pais[Defensor].Ejercitos:=EjerD;
    EnemigosQueSobreviven:=EjerD
  END; (* EnemigosQueSobreviven *)
(***********************************************************************************************************************)
(***********************************************************************************************************************)
FUNCTION EliminarSiMuerto(PJugador:byte):boolean;
  VAR
    pj,pm,PaisesVivos,UltPais : byte;
    AuxS                      : string;
  BEGIN
    EliminarSiMuerto:=False;
    PaisesVivos:=0;
    FOR pj:=1 TO 42 DO
      IF Juego.Pais[pj].Amo=PJugador THEN BEGIN UltPais:=pj;Inc(PaisesVivos)END;
    IF PaisesVivos=1 THEN
      BEGIN (* El tío la palmó, borrarlo del juego*)
        Juego.Pais[UltPais].Amo:=Juego.Poles[Juego.Actual];
        (* Robarle las cartas *)
        FOR pj:=1 TO Juego.Jugador[PJugador].NCartas DO
          Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj+
            Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas]:=
            Juego.Jugador[PJugador].LaCarta[pj];
        Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas,
            Juego.Jugador[PJugador].NCartas);
        IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0 THEN EnableWindow(Carta,True);
        (* Correr juego.Actual *)
        FOR pj:=1 TO 6 DO IF Juego.Poles[pj]=PJugador THEN
          BEGIN Juego.Poles[pj]:=0; pm:=pj END;
        IF pm<Juego.Actual THEN Dec(Juego.Actual);
        (* Correr poles *)
        FOR pj:=1 TO 5 DO IF Juego.Poles[pj]=0 THEN
          BEGIN
            FOR pm:=pj TO 5 DO Juego.Poles[pm]:=Juego.Poles[pm+1];
            Juego.Poles[6]:=0
          END;
        (* dibujar lo necesario e indicar la muerte *)
        PintarPais(UltPais,Negro);
        DibujarTurnos;
        EliminarSiMuerto:=True
      END (* PaisesVivos=1 *)
  END; (* EliminarSiMuerto *)
(*************************************************************************************)
(*************************************************************************************)
PROCEDURE ColocarSegunModalidad(Atacante,Defensor:byte);
  VAR
    AvanceAutomatico : boolean;
  BEGIN
    AvanceAutomatico:=Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut;
    IF Mensaje.WParam=MK_Shift THEN AvanceAutomatico:=NOT AvanceAutomatico;
    IF Juego.Poles[2]<>0 THEN (* Si no ganó ya *)
    IF ((AvanceAutomatico)OR(Juego.Pais[Atacante].Ejercitos<Juego.Jugador[Juego.Poles[Juego.Actual]].NDados))
      THEN (* AvanceAut *)
        BEGIN
          Juego.Pais[Defensor].Ejercitos:=Juego.Pais[Atacante].Ejercitos;
          Juego.Pais[Atacante].Ejercitos:=0;
          PintarPais(Atacante,Negro);
          PintarPais(Defensor,Borde); (* seleccionado el pais atacado *)
          Juego.PAtaca:=Defensor
        END
      ELSE (* Avance manual *)
        BEGIN
          Juego.Pais[Defensor].Ejercitos:=Juego.Jugador[Juego.Poles[Juego.Actual]].NDados-1;
          Juego.NumSold:=Juego.Pais[Atacante].Ejercitos-Juego.Jugador[Juego.Poles[Juego.Actual]].NDados-1;
          Juego.Pais[Atacante].Ejercitos:=0;
          PintarPais(Defensor,Negro);
          PintarPais(Atacante,Borde); (* seleccionado el pais atacante *)
          (* Cambio de estado *)
          (* Activar los botones necesarios *)
          EnableWindow(Hecho,False);
          EnableWindow(Fortificar,False);
          (* Pintar los soldados *)
          PintarSoldados(Juego.NumSold);
          (* Cambio del estado *)
          Juego.Estado:=Ganando
        END (* Avance manual *)
  END; (* ColocarSegunModalidad *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE MaximasCartas(PEntrada:TCarMarc; VAR PNumSalida:byte; VAR PSalida:TCarMarc);
(* matriz salida -> cartas a ser marcadas. PNumSalida -> cuantas cartas se marcan *)
  VAR
    Maximo           : byte;
    i,j,k,l,m,n      : byte;
    Comodines        : byte;
    NumSalida,d      : byte;
    NumComod         : ARRAY [1..6] of byte;
    Matriz,MatrizSol : TCarMarc;
  PROCEDURE CalculosCartas(PPEntrada:TCarMarc; VAR PPNumSalida:byte; VAR PPSalida:TCarMarc);
    VAR
      MatrizAux : TCarMarc;
      pj,pm,d   : Byte;
      NF,NTemp  : Byte;
      Entrar    : boolean;
      T,P       : ARRAY [1..3] OF Byte;
    PROCEDURE Coger(A,B,C:byte);
      VAR
        ca,cb,cc,pj : byte;
      BEGIN
        ca:=0;cb:=0;cc:=0;d:=1;
        FOR pj:=1 TO 9 DO
          BEGIN
            IF ((PPEntrada[pj].Tipo=T[1])AND(ca<A))THEN
              BEGIN Inc(ca);MatrizAux[d].LaCarta:=PPEntrada[pj].LaCarta;Inc(d) END;
            IF ((PPEntrada[pj].Tipo=T[2])AND(cb<B))THEN
              BEGIN Inc(cb);MatrizAux[d].LaCarta:=PPEntrada[pj].LaCarta;Inc(d) END;
            IF ((PPEntrada[pj].Tipo=T[3])AND(cc<C)) THEN
              BEGIN Inc(cc);MatrizAux[d].LaCarta:=PPEntrada[pj].LaCarta;Inc(d) END
          END
      END; (* Coger *)

    BEGIN (* CalculosCartas *)
      (* Inicialización *)
      d:=1;Entrar:=False;NF:=0;
      T[1]:=1;T[2]:=2;T[3]:=3;
      P[1]:=0;P[2]:=0;P[3]:=0;
      FOR pj:=1 TO 9 DO
        BEGIN MatrizAux[pj].Tipo:=0; MatrizAux[pj].LaCarta:=0 END;
      (* Almacenar nº de cartas por tipo y medir el nº de filas *)
      FOR pj:=1 TO 9 DO IF PPEntrada[pj].Tipo<>0 THEN Inc(P[PPEntrada[pj].Tipo]);
      IF ((P[1]>=1)AND(P[2]>=1)AND(P[3]>=1))THEN NF:=1;
      IF ((P[1]>=2)AND(P[2]>=2)AND(P[3]>=2))THEN NF:=2;
      IF ((P[1]>=3)AND(P[2]>=3)AND(P[3]>=3))THEN NF:=3;
      (* Arrimar columnas por ordenación burbuja *)
      WHILE NOT Entrar DO
        BEGIN
          Entrar:=True;
          FOR pj:=1 TO 2 DO IF P[pj]<P[pj+1] THEN
            BEGIN
              NTemp:=P[pj]; P[pj]:=P[pj+1];P[pj+1]:=NTemp;
              NTemp:=T[pj]; T[pj]:=T[pj+1];T[pj+1]:=NTemp;
              Entrar:=False
            END
        END; (* While *)
      (* Estudio de los casos posibles *)
      IF NF=3 THEN Coger(3,3,3);
      IF NF=2 THEN
        BEGIN
          IF P[1]=5 THEN Coger(5,2,2)
                    ELSE Coger(2,2,2)
        END; (* NF=2 *)
      IF NF=1 THEN
        BEGIN
          IF P[1]=7                       THEN Coger(7,1,1)
            ELSE IF ((P[1]=4)AND(P[2]=4)) THEN Coger(4,4,1)
            ELSE IF P[1]>3                THEN Coger(4,1,1)
            ELSE IF ((P[1]=3)AND(P[2]=3)) THEN Coger(3,3,0)
            ELSE                               Coger(1,1,1)
        END; (* NF=1 *)
      IF NF=0 THEN
        BEGIN
          IF P[1]=9                       THEN Coger(9,0,0)
            ELSE IF ((P[1]=6)AND(P[2]=3)) THEN Coger(6,3,0)
            ELSE IF P[1]>5                THEN Coger(6,0,0)
            ELSE IF ((P[1]>2)AND(P[2]>2)) THEN Coger(3,3,0)
            ELSE IF P[1]>2                THEN Coger(3,0,0)
        END; (* NF=0 *)
      PPNumSalida:=d-1;
      PPSalida:=MatrizAux
    END; (* CalculosCartas *)

  BEGIN (* MaximasCartas *)
    (* Inicialización *)
    FOR j:=1 TO 9 DO
      BEGIN
        Matriz[j].Tipo:=0;    MatrizSol[j].Tipo:=0;
        Matriz[j].LaCarta:=0; MatrizSol[j].LaCarta:=0
      END;
    FOR j:=1 TO 6 DO NumComod[j]:=0;
    Comodines:=0;Maximo:=0;d:=1;
    (* Creacion de la matriz a enviar a calculoscartas *)
    FOR j:=1 TO 9 DO IF PEntrada[j].Tipo=4
      THEN
        BEGIN
          Inc(Comodines);
          NumComod[Comodines]:=PEntrada[j].LaCarta
        END
      ELSE IF PEntrada[j].Tipo<>0 THEN
        BEGIN
          Matriz[d].Tipo:=PEntrada[j].Tipo;
          Matriz[d].LaCarta:=PEntrada[j].LaCarta;
          Inc(d)
        END;
    (* Contemplación de casos según nº de comodines *)
    IF Comodines=6
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
            FOR j:=i TO 3 DO
              FOR k:=j TO 3 DO
                FOR l:=k TO 3 DO
                  FOR m:=l TO 3 DO
                    FOR n:=m TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        Matriz[d+1].Tipo:=j; Matriz[d+1].LaCarta:=NumComod[2];
                        Matriz[d+2].Tipo:=k; Matriz[d+2].LaCarta:=NumComod[3];
                        Matriz[d+3].Tipo:=l; Matriz[d+3].LaCarta:=NumComod[4];
                        Matriz[d+4].Tipo:=m; Matriz[d+4].LaCarta:=NumComod[5];
                        Matriz[d+5].Tipo:=n; Matriz[d+5].LaCarta:=NumComod[6];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol;Maximo:=NumSalida END
                      END
        END (* THEN *)
    ELSE IF Comodines=5
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
            FOR j:=i TO 3 DO
              FOR k:=j TO 3 DO
                FOR l:=k TO 3 DO
                  FOR m:=l TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        Matriz[d+1].Tipo:=j; Matriz[d+1].LaCarta:=NumComod[2];
                        Matriz[d+2].Tipo:=k; Matriz[d+2].LaCarta:=NumComod[3];
                        Matriz[d+3].Tipo:=l; Matriz[d+3].LaCarta:=NumComod[4];
                        Matriz[d+4].Tipo:=m; Matriz[d+4].LaCarta:=NumComod[5];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol; Maximo:=NumSalida END
                      END
        END
    ELSE IF Comodines=4
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
            FOR j:=i TO 3 DO
              FOR k:=j TO 3 DO
                FOR l:=k TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        Matriz[d+1].Tipo:=j; Matriz[d+1].LaCarta:=NumComod[2];
                        Matriz[d+2].Tipo:=k; Matriz[d+2].LaCarta:=NumComod[3];
                        Matriz[d+3].Tipo:=l; Matriz[d+3].LaCarta:=NumComod[4];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol; Maximo:=NumSalida END
                      END
        END
    ELSE IF Comodines=3
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
            FOR j:=i TO 3 DO
              FOR k:=j TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        Matriz[d+1].Tipo:=j; Matriz[d+1].LaCarta:=NumComod[2];
                        Matriz[d+2].Tipo:=k; Matriz[d+2].LaCarta:=NumComod[3];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol; Maximo:=NumSalida END
                      END
        END
    ELSE IF Comodines=2
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
            FOR j:=i TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        Matriz[d+1].Tipo:=j; Matriz[d+1].LaCarta:=NumComod[2];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol; Maximo:=NumSalida END
                      END
        END
    ELSE IF Comodines=1
      THEN
        BEGIN
          FOR i:=1 TO 3 DO
                      BEGIN
                        Matriz[d].Tipo:=i;   Matriz[d].LaCarta:=NumComod[1];
                        CalculosCartas(Matriz,NumSalida,MatrizSol);
                        IF NumSalida>=Maximo THEN BEGIN PSalida:=MatrizSol; Maximo:=NumSalida END
                      END
        END
   ELSE CalculosCartas(Matriz,Maximo,PSalida); (* Comodines=0 *)
   PNumSalida:=Maximo
  END; (* MaximasCartas *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE MarcarCartas(PPDialogo:HWnd);
  VAR
    pj,pm,recibido : byte;
    MatrizSol      : TCarMarc;
  BEGIN
    (* Inicialización de variables *)
    FOR pj:=1 TO 9 DO
      BEGIN MatrizSol[pj].Tipo:=0;MatrizSol[pj].LaCarta:=0 END;
    FOR pj:=1 TO Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas DO
          BEGIN
            MatrizSol[pj].LaCarta:=Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj];
            MatrizSol[pj].Tipo:=Cartas[Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]].Tipo
          END;
    (* Escogimiento de las combinaciones máximas válidas *)
    MaximasCartas(MatrizSol,Recibido,MatrizSol);
    (* Activar los botones correspondientes *)
    IF Recibido>0
      THEN
        BEGIN
          FOR pj:=1 TO Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas DO
            FOR pm:=1 TO Recibido DO
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]=MatrizSol[pm].LaCarta
                THEN SendDlgItemMessage(PPDialogo,400+pj,BM_setCheck,1,0)
        END
      ELSE EnableWindow(GetDlgItem(PPDialogo,1),false)
  END;(* MarcarCartas *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE VoltearCartas(PDialogo:HWnd);
  VAR
    DCLocal,MemDC : HDC;
    pj            : byte;
    Ancho,x,y     : integer;
  BEGIN
    DCLocal:=GetDC(PDialogo);
    MemDC:=CreateCompatibleDC(DCLocal);
    SelectObject(MemDC,Dorso);
    SelectObject(DCLocal,GetStockObject(Gray_Brush));
    WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO
    FOR pj:=1 TO NCartas DO
      IF SendMessage(GetDlgItem(PDialogo,400+pj),BM_GetCheck,0,0)=0 THEN
        BEGIN
          CASE NCartas OF
            1,2,3 : BEGIN
                      Ancho:=458 DIV NCartas;
                      x:=Ancho*pj-(Ancho DIV 2);
                      y:=80
                    END;
            4,5,6 : BEGIN
                      Ancho:=388 DIV(NCartas-3);
                      IF pj<4 THEN BEGIN
                                     x:=152*pj-96;
                                     y:=10
                                   END
                              ELSE BEGIN
                                     x:=Ancho*(pj-3)-(Ancho DIV 2)+15;
                                     y:=190
                                   END
                    END;
            7     : BEGIN
                      IF pj<5 THEN BEGIN
                                     x:=137*pj-114;
                                     y:=10
                                   END
                              ELSE BEGIN
                                     x:=146*(pj-4)-105;
                                     y:=190
                                   END
                    END;
            8,9   : BEGIN
                      Ancho:=438 DIV(NCartas-5);
                      IF pj<6 THEN BEGIN
                                     x:=10+110*(pj-1);
                                     y:=10
                                   END
                              ELSE BEGIN
                                     x:=Ancho*(pj-5)-(Ancho DIV 2)-32;
                                     y:=190
                                   END
                    END
          END; (* Case NCartas *)
          RoundRect(DCLocal,x,y,x+102,y+152,15,9);
          BitBlt(DCLocal,x+26,y+69,49,14,MemDC,0,0,SRCCopy)
        END; (* FOR *)
    DeleteDC(MemDC)
  END; (* VoltearCartas *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE EliminarCartas(PMatriz:TCarMarc;PRecibido:byte);
(* Le borra al jugador actual las cartas de PMatriz que le pertenezcan *)
  VAR
    pj,pm,voltios : byte;
  BEGIN
    (* Cobrar los paises que se tengan *)
    FOR pj:=1 TO PRecibido DO
      IF PMatriz[pj].LaCarta<43 THEN
        IF Juego.Pais[PMatriz[pj].LaCarta].Amo=Juego.Poles[Juego.Actual] THEN
          IF Juego.Pais[PMatriz[pj].LaCarta].Ejercitos<98 THEN
            Inc(Juego.Pais[PMatriz[pj].LaCarta].Ejercitos,2);
    (* Borrar cartas ya cobradas *)
    FOR pj:=1 TO Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas DO
      FOR pm:=1 TO PRecibido DO
        IF Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]=PMatriz[pm].LaCarta
          THEN
            BEGIN
              Cartas[Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]].Amo:=0;
              Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]:=0;
              Dec(Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas)
            END;
    (* eliminación de ceros y correr las cartas de posicion *)
    pj:=1;Voltios:=0;
    WHILE pj<10 DO
      BEGIN
        IF Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]=0
          THEN
            BEGIN
              Inc(Voltios);
              IF Voltios<10
                THEN
                  FOR pm:=pj+1 TO 9 DO
                    Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pm-1]:=
                    Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pm]
                ELSE Inc(pj)
            END
          ELSE Inc(pj)
      END (* While *)
  END; (* EliminarCartas *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION ProcesarCombinacion(PPDialogo:HWnd):boolean;
  VAR
    MatrizIda,MatrizVuelta : TCarMarc;
    Recibido,pj,pm,d       : byte;
  BEGIN
    (* Inicializacion *)
    FOR pj:=1 TO 9 DO
      BEGIN MatrizIda[pj].Tipo:=0;MatrizIda[pj].LaCarta:=0 END;
    d:=0;
    (* Averiguar combinación marcada *)
    FOR pj:=1 TO Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas DO
      IF SendMessage(GetDlgItem(PPDialogo,400+pj),BM_GetCheck,0,0)=1 THEN
        BEGIN
          Inc(d);
          MatrizIda[d].Tipo:=Cartas[Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]].Tipo;
          MatrizIda[d].LaCarta:=Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]
        END;
    (* Contemplar los posibles casos *)
     IF d<>0
       THEN
         BEGIN
           (* Ver si vale *)
           MaximasCartas(MatrizIda,Recibido,MatrizVuelta);
           IF Recibido=d
             THEN
               BEGIN
                 IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas-d>4
                   THEN
                     BEGIN
                       MessageBox(PPDialogo,'No puedes tener mas de 4 cartas a la vez.','Error',
                         MB_IconStop OR MB_OK);
                       ProcesarCombinacion:=False
                     END
                   ELSE (* Aquí se cobra realmente *)
                     BEGIN
                       (* dar vuelta cartas no usadas y eliminar usadas *)
                       VoltearCartas(PPDialogo);
                       FOR pm:=1 TO 9 DO IF MatrizVuelta[pm].LaCarta<>0 THEN MessageBeep(1);
                       EliminarCartas(MatrizVuelta,Recibido);
                       (* Cobrar soldaditos *)
                       FOR pm:=1 TO d DIV 3 DO
                         BEGIN
                           Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar);
                           CASE Juego.Modalidad OF
                             1: BEGIN
                                  Inc(Juego.NumSold,5);
                                  Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,5)
                                END;
                             2: BEGIN
                                  Inc(Juego.NumSold,3+Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar);
                                  Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,
                                      3+Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)
                                END;
                             3: IF Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar<10
                                  THEN BEGIN
                                     Inc(Juego.NumSold,3+TRUNC(Elev(1.6,Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)));
                                         Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,
                                             3+TRUNC(Elev(1.6,Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)))
                                       END
                                  ELSE BEGIN
                                         Inc(Juego.NumSold,3+TRUNC(Elev(1.6,9)));
                                         Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,3+TRUNC(Elev(1.6,9)))
                                       END
                           END; (* Case *)
                           ProcesarCombinacion:=True (* Cerrar *)
                         END (* For *)
                     END
               END
             ELSE
               BEGIN (* Recibido<>d *)
                 MessageBox(PPDialogo,'La combinación escogida no es correcta','Error',
                   MB_IconStop OR MB_OK);
                 ProcesarCombinacion:=False (* No cerrar *)
               END
         END
       ELSE (* d=0 *)
         BEGIN
           IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>4
             THEN
               BEGIN
               MessageBox(PPDialogo,'No puedes tener mas de 4 cartas a la vez.','Error',
                  MB_IconStop OR MB_OK);
               ProcesarCombinacion:=False
               END
             ELSE ProcesarCombinacion:=True
         END
  END; (* ProcesarCombinacion *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE DarCarta(PJugador:byte);
  VAR
    pj,tantas:byte;
  PROCEDURE Repartir;
    VAR
      SelDC,Salvado,MemDC : HDC;
      a,b,c : integer;
    BEGIN
      (* Copiamos la pantalla antes de dibujar sobre ella *)
      SelDC:=CreateCompatibleDC(MDC);
      Salvado:=CreateCompatibleBitMap(MDC,638,440);
      Salvado:=SelectObject(SelDC,Salvado);
      BitBlt(SelDC,0,0,638,440,MDC,0,0,SRCCopy);
      (* Dibujamos sobre ella *)
      MemDC:=CreateCompatibleDC(MDC);
      SelectObject(MemDC,Dorso);
      SelectObject(MDC,getstockobject(Gray_Brush));
      FOR c:=1 TO 48 DO
        BEGIN
          a:=20+Random(515); b:=10+Random(250);
          RoundRect(MDC,a,b,a+102,b+152,15,9);
          BitBlt(MDC,a+26,b+69,49,14,MemDC,0,0,SRCCopy)
        END;
      FOR c:=1 TO 48 DO IF Cartas[c].Amo=0 THEN Cartas[c].Usada:=False;
      SelectObject(MDC,GetStockObject(White_Brush));
      (* Reponemos lo borrado *)
      BitBlt(MDC,0,0,638,440,SelDC,0,0,SRCCopy);
      (* Borramos objetos creados *)
      Salvado:=SelectObject(SelDC,Salvado);
      DeleteDC(SelDC);
      DeleteDC(MemDC);
      DeleteObject(Salvado)
    END; (* Repartir *)

  BEGIN
    Tantas:=0;
    FOR pj:=1 TO 48 DO IF Cartas[pj].Usada THEN Inc(Tantas);
    IF Tantas=48 THEN Repartir;
    REPEAT
      pj:=Random(48)+1;
    UNTIL (NOT Cartas[pj].Usada);
    (* Dar la carta *)
    Cartas[pj].Usada:=True;
    Cartas[pj].Amo  :=PJugador;
    Inc(Juego.Jugador[PJugador].NCartas);
    Juego.Jugador[PJugador].LaCarta[Juego.Jugador[PJugador].NCartas]:=pj;
    Juego.Invadio:=False
  END; (* DarCarta *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE EliminarPintarCartas(PDC:HDC;PCarMarc:TCarMarc;PRecibido:byte);
  VAR
    SelDC,Salvado : HDC;
    pj            : byte;
  BEGIN
    (* Copiamos la pantalla antes de dibujar sobre ella *)
    SelDC:=CreateCompatibleDC(PDC);
    Salvado:=CreateCompatibleBitMap(PDC,638,440);
    Salvado:=SelectObject(SelDC,Salvado);
    BitBlt(SelDC,0,0,638,440,PDC,0,0,SRCCopy);
    (* Dibujamos sobre ella *)
    FOR pj:=1 TO 6 DO IF PCarMarc[pj].LaCarta<>0 THEN DibujarCarta(PDC,10+105*(pj-1),50,PCarMarc[pj].LaCarta);
    FOR pj:=7 TO 9 DO IF PCarMarc[pj].LaCarta<>0 THEN DibujarCarta(PDC,60+105*(pj-7),208,PCarMarc[pj].LaCarta);
    FOR pj:=1 TO 9 DO IF PCarMarc[pj].LaCarta<>0 THEN MessageBeep(1);
    (* Eliminamos las cartas cobradas *)
    EliminarCartas(PCarMarc,PRecibido);
    (* Reponemos la parte de la pantalla que se halla jodido *)
    IF PCarMarc[9].LaCarta<>0
      THEN BitBlt(PDC,9,49,638,315,SelDC,9,49,SRCCopy)
      ELSE IF PCarMarc[6].LaCarta<>0
              THEN BitBlt(PDC,9,49,638,155,SelDC,9,49,SRCCopy)
              ELSE BitBlt(PDC,9,49,325,155,SelDC,9,49,SRCCopy);
    (* Borramos objetos creados *)
    Salvado:=SelectObject(SelDC,Salvado);
    DeleteDC(SelDC);
    DeleteObject(Salvado)
  END; (* EliminarPintarCartas *)
(**************************************************************************************)






(**************************************************************************************)
(* Función que maneja la opción Partida Nueva del menu *)
FUNCTION PartidaNueva(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    NumJugadores,pj,pk : integer;
    MemDC,DCLocal      : HDC;
    Incremento         : shortint; (* Hacia donde se mueve cambiacolor *)
  BEGIN (* PartidaNueva *)
    PartidaNueva := True;
    CASE PMensaje OF
      wm_InitDialog:
        BEGIN (* WM_InitDialog *)
          (* Inicialización de los estados "por defecto" de los botones autoradio *)
          SendDlgItemMessage(PDialogo,115,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,125,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,135,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,145,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,155,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,165,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,171,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,181,BM_setCheck,1,0);
          (* Colocar los dibujos de los cuatro tipos de jugador *)
          ShowWindow(PDialogo,SW_ShowNormal);
          DCLocal :=GetDC(PDialogo);
          MemDC   :=CreateCompatibleDC(DCLocal);
          SelectObject(memDC,Tu);
          BitBlt(DCLocal,258,2,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Macho);
          BitBlt(DCLocal,303,3,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Caos);
          BitBlt(DCLocal,351,3,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Neutral);
          BitBlt(DCLocal,399,3,42,42,memDC,0,0,SRCCopy);
          DeleteDC(MemDC);
          (* Colores de los juagadores *)
          FOR pj:=1 TO 6 DO
            BEGIN
              SelectObject(DCLocal,Brocha[Juego.Jugador[pj].Color].RGB);
              Rectangle(DCLocal,6,14+pj*30,46,38+pj*30);
            END;(* For *)
          ReleaseDC(PDialogo,DCLocal)
        END; (* WM_InitDialog *)
      wm_Command:
        CASE PWParam OF
          ID_OK:
            BEGIN
              (* Recuperar el contenido del diálogo *)
              FOR pj:=1 TO 6 DO WITH Juego.Jugador[pj] DO
                BEGIN
                  Nombre[0]:=chr(GetDlgItemText(PDialogo,190+pj,@Nombre[1],23));
                  IF SendMessage(GetDlgitem(PDialogo,101+(pj*10)),BM_GetCheck,0,0)=1 THEN Modalidad := 1;  (*Jugador*)
                  IF SendMessage(GetDlgitem(PDialogo,102+(pj*10)),BM_GetCheck,0,0)=1 THEN Modalidad := 2;  (*Macho man*)
                  IF SendMessage(GetDlgitem(PDialogo,103+(pj*10)),BM_GetCheck,0,0)=1 THEN Modalidad := 3;  (*Caótico*)
                  IF SendMessage(GetDlgitem(PDialogo,104+(pj*10)),BM_GetCheck,0,0)=1 THEN Modalidad := 4;  (*Neutral*)
                  IF SendMessage(GetDlgitem(PDialogo,105+(pj*10)),BM_GetCheck,0,0)=1 THEN Modalidad := 5   (*No juega*)
                END; (* With *)
              (* comprobación de que hay al menos dos jugadores y creación poles *)
              NumJugadores:=0;
              FOR pj:=1 TO 6 DO IF Juego.Jugador[pj].Modalidad<>5 THEN
                BEGIN
                  Inc(NumJugadores);
                  Juego.Poles[NumJugadores]:=Pj
                END;
              Juego.Actual:=1; (* El jugador actual es el que ocupa la primera posición de la parrilla o poles *)
              IF NumJugadores<2 THEN
                BEGIN
                  MessageBeep(MB_IconAsterisk);
                  EXIT (* no se hace nada, y el dialogo continua igual *)
                END;
              (* Continuando la recuperación de datos del diálogo *)
              IF SendMessage(GetDlgitem(PDialogo,171),BM_GetCheck,0,0)=1 THEN Juego.Modalidad := 1; (* 5,5,5...*)
              IF SendMessage(GetDlgitem(PDialogo,172),BM_GetCheck,0,0)=1 THEN Juego.Modalidad := 2; (* 4,5,6,7...*)
              IF SendMessage(GetDlgitem(PDialogo,173),BM_GetCheck,0,0)=1 THEN Juego.Modalidad := 3; (* 4,6,8,10,15...*)
              IF SendMessage(GetDlgitem(PDialogo,181),BM_GetCheck,0,0)=1
                THEN Aleatorio:=True
                ELSE Aleatorio:=False;
              (* Desmarcaje de los colores no utilizados y/o sobrantes *)
              FOR pj:=1 TO MaxDibu DO Brocha[pj].Usada:=False;
              FOR pj:=1 TO 6  DO IF Juego.Poles[pj]<>0 THEN Brocha[Juego.Jugador[Juego.Poles[pj]].Color].Usada:=True;
              (* Activación del juego *)
              Juego.Activo := True;
              (* Finalización del dialogo *)
              EndDialog(PDialogo,1)
            END; (* ID_OK *)
          ID_Cancel:
            BEGIN
              EndDialog(PDialogo,2)
            END
          ELSE PartidaNueva := False
        END; (* case WParam of *)
      WM_LButtonUp:
        BEGIN
          IF ((LoWord(PLParam)>7) AND (LoWord(PLParam)<45)) THEN
            BEGIN
              IF LoWord(PLParam)<26 THEN Incremento:=-1
                                    ELSE Incremento:=1;
              DCLocal:=GetDC(PDialogo);
              CASE HiWord(PLParam) OF
                45..67  : CambiarColor(1,6,44 ,46,68 ,Incremento,DCLocal);
                75..97  : CambiarColor(2,6,74 ,46,98 ,Incremento,DCLocal);
                105..127: CambiarColor(3,6,104,46,128,Incremento,DCLocal);
                135..157: CambiarColor(4,6,134,46,158,Incremento,DCLocal);
                165..187: CambiarColor(5,6,164,46,188,Incremento,DCLocal);
                195..217: CambiarColor(6,6,194,46,218,Incremento,DCLocal)
                ELSE PartidaNueva := False (* Para que windows procese el mensaje *)
              END; (* Case *)
              ReleaseDC(PDialogo,DCLocal)
            END (* If *)
          ELSE PartidaNueva := False;
        END (* Wm_Lbuttondown *)
      ELSE PartidaNueva := False
    END (* Case Message of *)
  END; (* Función PartidaNueva *)
(**************************************************************************************)
(**************************************************************************************)
(* Función que maneja la opcion del menu Ficheros_Abrir *)
FUNCTION AbrirFichero(PDialogo: HWnd; PMensaje, PWParam: Word; PLParam: Longint): Bool; export;
  VAR
    Pj        : byte;
    Busqueda  : boolean;
    Aspirante : TJuego;
  BEGIN (* AbrirFichero *)
    AbrirFichero:=True; (* Se presupone que se procesa el mensaje *)
    CASE PMensaje OF
      WM_InitDialog:
        BEGIN
          GetCurDir(@Directorio[1],0);
          LStrCat(@Directorio[1],'\');
          LStrCat(@Directorio[1],@NomFich[1]);
          DlgDirList(PDialogo,@Directorio[1],101,104,$0020);
          DlgDirList(PDialogo,@Directorio[1],102,0,$C010);
          SetDlgItemText(PDialogo,103,@NomFich[1])
        END;
      WM_Command:
        CASE PWParam OF
          101: (* Lista de ficheros *)
            BEGIN
              DlgDirSelect(PDialogo,@NomFich[1],101);
              SetDlgItemText(PDialogo,103,@NomFich[1]);
              IF HiWord(PLParam)=LBn_DblClk THEN SendMessage(PDialogo,WM_Command,ID_OK,0)
            END;
          102: (* Lista de unidades y directorios *)
            BEGIN
              IF HiWord(PLParam)=LBn_DblClk THEN
                BEGIN
                  DlgDirSelect(PDialogo,@Directorio[1],102);
                  LStrCat(@Directorio[1],@NomFich[1]);
                  SetDlgItemText(PDialogo,103,@NomFich[1]);
                  DlgDirList(PDialogo,@Directorio[1],101,104,$0020);
                  DlgDirList(PDialogo,@Directorio[1],102,0,$C010)
                END
            END;
          ID_OK:
            BEGIN
              Busqueda:=False;
              GetDlgItemText(PDialogo,103,@NomFich[1],128);
              FOR Pj:=1 TO LStrLen(@NomFich[1]) DO IF NomFich[Pj] IN ['?','*','\',':'] THEN Busqueda:=True;
              IF Busqueda
                THEN SendMessage(PDialogo,WM_InitDialog,0,0)
                ELSE
                  BEGIN
                    GetCurDir(@Directorio[1],0);
                    Directorio[0]:=Char(LStrLen(@Directorio[1]));
                    NomFich[0]:=Char(LStrLen(@NomFich[1]));
                    NomFich:=Directorio+'\'+NomFich;
                    Assign(Fichero,NomFich);
                    (* $I- *)
                    ReSet(Fichero);
                    (*$I+*)
                    IF IOResult<>0
                      THEN
                        BEGIN
                          MessageBox(PDialogo,'        Error al abrir el archivo,'+Chr(13)+
                                             +'inténtelo de nuevo o cancele la operación.','ERROR',MB_IconStop OR MB_OK);
                          SetFocus(GetDlgItem(PDialogo,103))
                        END
                      ELSE
                        BEGIN
                          (*$I-*)
                          Read(Fichero,Aspirante);
                          (*$I+*)
                          IF IOResult<>0
                            THEN
                              BEGIN
                                MessageBox(PDialogo,'        Error al abrir el archivo,'+Chr(13)+
                                                    'inténtelo de nuevo o cancele la operación.','ERROR',MB_IconStop OR MB_OK);
                                SetFocus(GetDlgItem(PDialogo,103))
                              END
                            ELSE
                              BEGIN
                                Close(Fichero);
                                IF Aspirante.DNI<>NIF
                                  THEN
                                    BEGIN
                                      MessageBox(PDialogo,'    El formato del archivo es incorrecto,'+Chr(13)+
                                                          'inténtelo de nuevo o cancele la operación.',
                                                          'ERROR',MB_IconStop OR MB_OK);
                                      SetFocus(GetDlgItem(PDialogo,103))
                                    END
                                  ELSE
                                    BEGIN
                                      Juego:=Aspirante;
                                      EndDialog(PDialogo,1)
                                    END
                              END
                        END
                  END (* Else IF Busqueda *)
            END;(* ID_OK *)
          ID_Cancel:
            BEGIN
              Directorio:='';
              NomFich:='';
              EndDialog(PDialogo,2)
            END
          ELSE AbrirFichero:=False
        END (* Case PWParam OF *)
      ELSE AbrirFichero:=False
    END (* Case PMensaje of *)
  END; (* AbrirFichero *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION SalvarComo(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    pp       : byte;
    Busqueda : boolean;
  BEGIN
    SalvarComo:=True;
    CASE PMensaje OF
      WM_InitDialog:
        BEGIN
          GetCurDir(@Directorio[1],0);
          LStrCat(@Directorio[1],'\*.rsk');
          DlgDirList(PDialogo,@Directorio[1],101,102,$C030);
          SetDlgItemText(PDialogo,103,'*.rsk')
        END;
      WM_Command:
        CASE PWParam OF
          101: (* Si se selecciona un fichero de la lista *)
            BEGIN
              DlgDirSelect(PDialogo,@Directorio[1],101);
              GetDlgItemText(PDialogo,103,@NomFich[1],128);
              IF HiWord(PLParam)=LBn_DblClk
                THEN
                  IF (Directorio[LStrLen(@Directorio[1])] IN ['\',':'])
                    THEN
                      BEGIN
                        LStrCat(@Directorio[1],@NomFich[1]);
                        DlgDirList(PDialogo,@Directorio[1],101,102,$C030)
                      END
                    ELSE SendMessage(PDialogo,WM_Command,ID_OK,0)
                ELSE
                  IF NOT (Directorio[LStrLen(@Directorio[1])] IN ['\',':'])
                    THEN SetDlgItemText(PDialogo,103,@Directorio[1])
            END;
          ID_OK:
            BEGIN
              Busqueda:=False;
              GetDlgItemText(PDialogo,103,@NomFich[1],128);
              GetCurDir(@Directorio[1],0);
              FOR Pp:=1 TO LStrLen(@NomFich[1]) DO IF NomFich[Pp] IN ['?','*','\',':'] THEN Busqueda:=True;
              Directorio[0]:=Char(LStrLen(@Directorio[1]));
              NomFich[0]:=Char(LStrLen(@NomFich[1]));
              NomFich:=Directorio+'\'+NomFich;
              IF Busqueda
                THEN DlgDirList(PDialogo,@NomFich[1],101,102,$C030)
                ELSE
                  BEGIN
                    Assign(Fichero,NomFich);
                    (*$I-*)
                    ReSet(Fichero);
                    (*$I+*)
                    IF IOResult=0 THEN
                      IF MessageBox(PDialogo,'El archivo indicado ya existe.'+Chr(13)+
                                            +'         ¿Sobreescribirlo?','Aviso',MB_IconAsterisk OR MB_YesNo)
                        =ID_No THEN BEGIN
                                      SetFocus(GetDlgItem(PDialogo,103));
                                      Exit
                                    END;
                    (*$I-*)
                    ReWrite(Fichero);
                    (*$I+*)
                    IF IOResult<>0
                      THEN
                        BEGIN
                          MessageBox(PDialogo,'      Error en la creación del archivo,'+Chr(13)+
                                             +'inténtelo de nuevo o cancele la operación.','ERROR',MB_IconStop OR MB_OK);
                          SetFocus(GetDlgItem(PDialogo,103))
                        END
                      ELSE
                        BEGIN
                          Write(Fichero,Juego);
                          Close(Fichero);
                          EndDialog(PDialogo,1)
                        END
                  END (* Begin del else *)
            END;(* ID_OK *)
          ID_Cancel:
            BEGIN
              EndDialog(PDialogo,2)
            END
          ELSE SalvarComo:=False
        END (* Case PWParam *)
      ELSE SalvarComo:=False
    END(* Case PMensaje *)
  END; (* SalvarComo *)
(**************************************************************************************)
(**************************************************************************************)
(* Función que realiza las operaciones del menu Acciones_Opciones de combate *)
FUNCTION OpcionesCombate(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    Cod : boolean;
  BEGIN
    OpcionesCombate := True; (* Se presupone que se va a procesar el mensaje *)
    CASE PMensaje OF
      WM_InitDialog:
        BEGIN
          A1:=Juego.Jugador[Juego.Poles[Juego.Actual]].Lucha;
          A2:=Juego.Jugador[Juego.Poles[Juego.Actual]].NDados;
          A3:=Juego.Jugador[Juego.Poles[Juego.Actual]].Minimo;
          A4:=Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut;
          SendDlgItemMessage(PDialogo,100+Juego.Jugador[Juego.Poles[Juego.actual]].Lucha,BM_setCheck,1,0);
          SendDlgItemMessage(PDialogo,104+Juego.Jugador[Juego.Poles[Juego.actual]].NDados,BM_setCheck,1,0);
          IF Juego.Jugador[Juego.Poles[Juego.actual]].AvanceAut
            THEN SendDlgItemMessage(PDialogo,109,BM_setCheck,1,0)
            ELSE SendDlgItemMessage(PDialogo,108,BM_setCheck,1,0);
          SetDlgItemInt(PDialogo,104,Juego.Jugador[Juego.Poles[Juego.Actual]].Minimo,False);
          ShowWindow(Pdialogo,SW_ShowNormal)
        END;
      WM_Command:
        CASE PWParam OF
          ID_OK:
            BEGIN
              WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO
                BEGIN
                  Minimo:=GetDlgItemInt(PDialogo,104,@Cod,False);
                  IF NOT Cod THEN Minimo:=A3;
                  IF SendMessage(GetDlgitem(PDialogo,101),BM_GetCheck,0,0)=1
                    THEN Lucha:=1
                    ELSE IF SendMessage(GetDlgitem(PDialogo,102),BM_GetCheck,0,0)=1
                      THEN Lucha:=2
                      ELSE Lucha:=3;
                  IF SendMessage(GetDlgitem(PDialogo,105),BM_GetCheck,0,0)=1
                    THEN NDados:=1
                    ELSE IF SendMessage(GetDlgitem(PDialogo,106),BM_GetCheck,0,0)=1
                      THEN NDados:=2
                      ELSE NDados:=3;
                  IF SendMessage(GetDlgitem(PDialogo,108),BM_GetCheck,0,0)=1
                    THEN AvanceAut:=False
                    ELSE AvanceAut:=True;
                END;
              EndDialog(PDialogo,1)
            END;
          ID_Cancel:
            BEGIN
              Juego.Jugador[Juego.Poles[Juego.Actual]].Lucha     :=A1;
              Juego.Jugador[Juego.Poles[Juego.Actual]].NDados    :=A2;
              Juego.Jugador[Juego.Poles[Juego.Actual]].Minimo    :=A3;
              Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut :=A4;
              EndDialog(PDialogo,2)
            END
          END(* Case PWParam of *)
      ELSE OpcionesCombate := False
    END (* Case PMensaje of *)
  END; (* Opciones_Comate *)
(**************************************************************************************)
(**************************************************************************************)
(* Función que maneja la opción estadísticas del menú acción *)
FUNCTION Estadisticas(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    DCLocal,MemDC   : HDC;
    pm,pj           : byte;
    NPa,Eje,Imp,Max : integer; (* NumeroPaises, Ejercitos, Impuestos *)
  BEGIN
    Estadisticas := True; (* Por defecto se supone que se procesa *)
    CASE PMensaje OF
      WM_initdialog:
        BEGIN
          (* Grabación de los colores iniciales por si se cancela *)
          FOR pm:=1 TO 6 DO OldColor[pm]:=Juego.Jugador[pm].Color;
          (* Inicializar botones de radio y poles de la parrilla de salida *)
          FOR pm:=1 TO 6 DO
            IF Juego.Poles[pm]<>0 (* <>0 cuando es jugador *)
              THEN SendDlgItemMessage(PDialogo,100+(10*Pm)+Juego.Jugador[Juego.Poles[pm]].Modalidad,BM_setCheck,1,0)
              ELSE (* Borrar los controles que no se usan y poner a cero sus poles *)
                FOR Pj:=1 TO 4 DO ShowWindow(GetDlgItem(PDialogo,100+(10*Pm)+Pj),SW_Hide);
          (* Colocar los dibujos de los cuatro tipos de jugador *)
          DCLocal := GetDC(PDialogo);
          MemDC := CreateCompatibleDC(DCLocal);
          ShowWindow(PDialogo,SW_ShowNormal);
          SelectObject(memDC,Tu);
          BitBlt(DCLocal,20,15,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Macho);
          BitBlt(DCLocal,77,16,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Caos);
          BitBlt(DCLocal,20,62,42,42,memDC,0,0,SRCCopy);
          SelectObject(memDC,Neutral);
          BitBlt(DCLocal,77,62,42,42,memDC,0,0,SRCCopy);
          (* Liberación del contexto grafico *)
          DeleteDC(MemDC);
          ReleaseDC(PDialogo,DCLocal);
          (* Dibujar los cuadros con los colores para los jugadores *)
          FOR pm:=1 TO 6 DO IF Juego.Poles[pm]<>0 THEN
            BEGIN
              SelectObject(DCLocal,Brocha[Juego.Jugador[Juego.Poles[pm]].Color].RGB);
              Rectangle(DCLocal,90+(60*pm),12,130+(60*pm),60)
            END;
          (* Escritura de las estadísticas *)
          FOR pm:=1 TO 6 DO IF Juego.Poles[pm]<>0 THEN WITH Juego.Jugador[Juego.Poles[pm]] DO
            BEGIN
              (* Cálculo del nº de paises y ejércitos *)
              NPa:=0; Eje:=0; Max:=0;
              FOR pj:=1 TO 42 DO IF Juego.Pais[pj].Amo=Juego.Poles[pm] THEN
                BEGIN
                  NPa:=NPa+1;
                  IF Juego.Pais[pj].Ejercitos>Max THEN Max:=Juego.Pais[pj].Ejercitos;
                  IF Juego.Pais[pj].Fortif>0 THEN Eje:=Eje+Juego.Pais[pj].Fortif;
                  Eje:=Eje+Juego.Pais[pj].Ejercitos
                END;
              (* Cálculo de los impuestos *)
              Imp:=TotalCobrar(Juego.Poles[pm]);
              (* Escribir las cosas en pantalla *)
              SetDlgItemInt(PDialogo,10*pm+1,NPa,False);
              SetDlgItemInt(PDialogo,10*pm+2,Eje,False);
              SetDlgItemInt(PDialogo,10*pm+3,Imp,False);
              SetDlgItemInt(PDialogo,10*pm+4,Total,False);
              SetDlgItemInt(PDialogo,10*pm+5,Victorias,False);
              SetDlgItemInt(PDialogo,10*pm+6,Derrotas,False);
              SetDlgItemInt(PDialogo,10*pm+7,(Max*100)DIV 99,False);
              SetDlgItemInt(PDialogo,10*pm+8,((Eje DIV Npa)*100)DIV 99,False)
            END (* Del WITH y del For Pm:=1 TO 6 *)
        END; (* WM_InitDialog *)
      WM_Command:
        BEGIN
          CASE PWParam OF
            ID_OK :
              BEGIN
                FOR pm:=1 TO 6 DO IF Juego.Poles[pm]<>0 THEN
                  BEGIN
                    IF SendMessage(GetDlgitem(PDialogo,101+(pm*10)),BM_GetCheck,0,0)=1 THEN
                      Juego.Jugador[Juego.Poles[pm]].Modalidad := 1;  (*Jugador*)
                    IF SendMessage(GetDlgitem(PDialogo,102+(pm*10)),BM_GetCheck,0,0)=1 THEN
                      Juego.Jugador[Juego.Poles[pm]].Modalidad := 2;  (*Macho man*)
                    IF SendMessage(GetDlgitem(PDialogo,103+(pm*10)),BM_GetCheck,0,0)=1 THEN
                      Juego.Jugador[Juego.Poles[pm]].Modalidad := 3;  (*Caótico*)
                    IF SendMessage(GetDlgitem(PDialogo,104+(pm*10)),BM_GetCheck,0,0)=1 THEN
                      Juego.Jugador[Juego.Poles[pm]].Modalidad := 4  (*Neutral*)
                  END;
                EndDialog(PDialogo,1)
              END; (* Id_Ok *)
            ID_Cancel:
              BEGIN
                FOR pm:=1 TO 6 DO Juego.Jugador[pm].Color:=OldColor[pm];
                HuboCambio:=False;
                EndDialog(PDialogo,2)
              END
            ELSE Estadisticas := False
          END (* Case PWParam *)
        END; (* WM_Command *)
      WM_LButtonUp:
        BEGIN
          IF ((HiWord(PLParam)>12) AND (HiWord(PLParam)<60)) THEN
            BEGIN
              DCLocal:=GetDC(PDialogo);
              CASE LoWord(PLParam) OF
                150..170: IF Juego.Poles[1]<>0 THEN CambiarColor(Juego.Poles[1],150,12,190,60,-1,DCLocal);
                171..190: IF Juego.Poles[1]<>0 THEN CambiarColor(Juego.Poles[1],150,12,190,60,+1,DCLocal);
                210..230: IF Juego.Poles[2]<>0 THEN CambiarColor(Juego.Poles[2],210,12,250,60,-1,DCLocal);
                231..250: IF Juego.Poles[2]<>0 THEN CambiarColor(Juego.Poles[2],210,12,250,60,+1,DCLocal);
                270..290: IF Juego.Poles[3]<>0 THEN CambiarColor(Juego.Poles[3],270,12,310,60,-1,DCLocal);
                291..310: IF Juego.Poles[3]<>0 THEN CambiarColor(Juego.Poles[3],270,12,310,60,+1,DCLocal);
                330..350: IF Juego.Poles[4]<>0 THEN CambiarColor(Juego.Poles[4],330,12,370,60,-1,DCLocal);
                351..370: IF Juego.Poles[4]<>0 THEN CambiarColor(Juego.Poles[4],330,12,370,60,+1,DCLocal);
                390..410: IF Juego.Poles[5]<>0 THEN CambiarColor(Juego.Poles[5],390,12,430,60,-1,DCLocal);
                411..430: IF Juego.Poles[5]<>0 THEN CambiarColor(Juego.Poles[5],390,12,430,60,+1,DCLocal);
                450..470: IF Juego.Poles[6]<>0 THEN CambiarColor(Juego.Poles[6],450,12,490,60,-1,DCLocal);
                471..490: IF Juego.Poles[6]<>0 THEN CambiarColor(Juego.Poles[6],450,12,490,60,+1,DCLocal)
                ELSE Estadisticas := False
              END; (* Case *)
              ReleaseDC(PDialogo,DCLocal)
            END (* If *)
          ELSE Estadisticas := False;
        END (* Wm_Lbuttondown *)
      ELSE Estadisticas := False
    END (* Case PMensaje of *)
  END; (* Estadísticas *)
(**************************************************************************************)
(**************************************************************************************)
(* Función que maneja la opcion del menu Ayuda_Sobre *)
FUNCTION Sobre(PDialogo: HWnd; PMensaje, PWParam: Word; PLParam: Longint): Bool; export;
  BEGIN (* Sobre *)
    Sobre := True;
    CASE PMensaje OF
      wm_InitDialog:
        BEGIN
          ShowWindow(PDialogo,SW_ShowNormal)
        END;
      wm_Command:
        IF PWParam=ID_OK THEN EndDialog(PDialogo, 1) ELSE Sobre := False
      ELSE Sobre := False
    END (* Case PMensaje of *)
  END; (* Función Sobre *)
(**************************************************************************************)
(**************************************************************************************)
(* Maneja la operación de cartas *)
FUNCTION ProcesarCartas(PDialogo:HWnd; PMensaje,PWParam:Word; PLParam:Longint):Bool; EXPORT;
  VAR
    DCLocal    : HDC;
    pz,Ancho   : integer;
    NPaises,c  : byte;
    NEjercitos : integer;
  BEGIN
    ProcesarCartas := True;
    CASE PMensaje OF
      WM_InitDialog:
        BEGIN
          WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
          CASE NCartas OF
            1,2,3 : BEGIN
                      Ancho:=458 DIV NCartas;
                      FOR pz:=1 TO NCartas DO
                      Cartas[LaCarta[pz]].Boton:=CreateWindow(
                        'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                        45+Ancho*(pz)-(Ancho DIV 2),236,13,13,PDialogo,400+pz,Instancia,nil);
                      ShowWindow(PDialogo,SW_ShowNormal);
                      DCLocal:=GetDC(PDialogo);
                      FOR pz:=1 TO NCartas DO DibujarCarta(DCLocal,Ancho*pz-(Ancho DIV 2),80,LaCarta[pz]);
                      ReleaseDC(PDialogo,DCLocal)
                    END;
            4,5,6 : BEGIN
                      FOR pz:=1 TO 3 DO
                      Cartas[LaCarta[pz]].Boton:=CreateWindow(
                        'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                        152*(pz)-51,166,13,13,PDialogo,400+pz,Instancia,nil);
                      Ancho:=388 DIV(NCartas-3);
                      FOR pz:=4 TO NCartas DO
                      Cartas[LaCarta[pz]].Boton:=CreateWindow(
                        'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                        60+Ancho*(pz-3)-(Ancho DIV 2),346,13,13,PDialogo,400+pz,Instancia,nil);
                      ShowWindow(PDialogo,SW_ShowNormal);
                      DCLocal:=GetDC(PDialogo);
                      FOR pz:=1 TO 3 DO DibujarCarta(DCLocal,152*pz-96,10,LaCarta[pz]);
                      FOR pz:=4 TO NCartas DO DibujarCarta(DCLocal,Ancho*(pz-3)-(Ancho DIV 2)+15,190,LaCarta[pz]);
                      ReleaseDC(PDialogo,DCLocal)
                    END;
            7     : BEGIN
                      FOR pz:=1 TO 4 DO
                        Cartas[LaCarta[pz]].Boton:=CreateWindow(
                          'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                          137*(pz)-69,166,13,13,PDialogo,400+pz,Instancia,nil);
                      FOR pz:=5 TO NCartas DO
                        Cartas[LaCarta[pz]].Boton:=CreateWindow(
                        'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                        146*(pz-4)-60,346,13,13,PDialogo,400+pz,Instancia,nil);
                      ShowWindow(PDialogo,SW_ShowNormal);
                      DCLocal:=GetDC(PDialogo);
                      FOR pz:=1 TO 4 DO DibujarCarta(DCLocal,137*pz-114,10,LaCarta[pz]);
                      FOR pz:=5 TO NCartas DO DibujarCarta(DCLocal,146*(pz-4)-105,190,LaCarta[pz]);
                      ReleaseDC(PDialogo,DCLocal)
                    END;
            8,9   : BEGIN
                      FOR pz:=1 TO 5 DO
                        Cartas[LaCarta[pz]].Boton:=CreateWindow(
                          'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                          55+110*(pz-1),166,13,13,PDialogo,400+pz,Instancia,nil);
                      Ancho:=438 DIV(NCartas-5);
                      FOR pz:=6 TO NCartas DO
                        Cartas[LaCarta[pz]].Boton:=CreateWindow(
                        'BUTTON',nil,WS_Child OR WS_Visible OR BS_AutoCheckBox,
                        13+Ancho*(pz-5)-(Ancho DIV 2),346,13,13,PDialogo,400+pz,Instancia,nil);
                      ShowWindow(PDialogo,SW_ShowNormal);
                      DCLocal:=GetDC(PDialogo);
                      FOR pz:=1 TO 5 DO DibujarCarta(DCLocal,10+110*(pz-1),10,LaCarta[pz]);
                      FOR pz:=6 TO NCartas DO DibujarCarta(DCLocal,Ancho*(pz-5)-(Ancho DIV 2)-32,190,LaCarta[pz]);
                      ReleaseDC(PDialogo,DCLocal)
                    END
          END (* Case NCartas *)
          END; (* With Juego.Jugador... *)
          IF (Juego.Estado IN [Repartiendo,Ganando])
            THEN MarcarCartas(PDialogo)  (* poner cruces en combinación por defecto *)
            ELSE EnableWindow(GetDlgItem(PDialogo,1),False) (* desactivar el OK *)
        END; (* WM_InitDlg *)
      WM_Command:
        CASE PWParam OF
          ID_OK:
            BEGIN
              IF ProcesarCombinacion(PDialogo) THEN
                BEGIN
                  (* Comprobar saturación de ejercitos *)
                  NPaises:=0;NEjercitos:=0;
                  FOR c:=1 TO 42 DO IF Juego.Pais[c].Amo=Juego.Poles[Juego.Actual] THEN Inc(NPaises);
                  FOR c:=1 TO 42 DO IF Juego.Pais[c].Amo=Juego.Poles[Juego.Actual]
                    THEN Inc(NEjercitos,Juego.Pais[c].Ejercitos);
                  IF NEjercitos+Juego.NumSold>NPaises*99 THEN Juego.NumSold:=(99*NPaises)-NEjercitos;
                  (* Indicar la salida según se está o no saturado *)
                  IF Juego.NumSold>0 THEN EndDialog(PDialogo,1)
                               ELSE BEGIN
                                      Juego.Estado:=Atacando;
                                      EndDialog(PDialogo,2)
                                    END
                END
            END; (* OK *)
          ID_CANCEL:
            BEGIN
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>4
                THEN MessageBox(PDialogo,'No puedes tener mas de 4 cartas a la vez.','Error',
                                MB_IconStop OR MB_OK)
                ELSE EndDialog(PDialogo,2)
            END
          ELSE ProcesarCartas := False
        END(* Case PWParam of *)
      ELSE ProcesarCartas := False
    END (* Case PMensaje of *)
  END; (* ProcesarCartas *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE CCOrdenador; (* Cobrar Cartas Ordenador *)
  VAR
    MatrizIda,MatrizVuelta : TCarMarc;
    Recibido,pj,d          : byte;
    NPaises,NEjercitos     : byte;
  BEGIN (* CCOrdenador *)
    (* Inicializacion *)
    FOR pj:=1 TO 9 DO
      BEGIN MatrizIda[pj].Tipo:=0;MatrizIda[pj].LaCarta:=0 END;
    d:=0;
    (* Meter las cartas en la matriz *)
    FOR pj:=1 TO Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas DO
      BEGIN
        Inc(d);
        MatrizIda[d].Tipo:=Cartas[Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]].Tipo;
        MatrizIda[d].LaCarta:=Juego.Jugador[Juego.Poles[Juego.Actual]].LaCarta[pj]
      END;
    (* Buscar combinaciones *)
    MaximasCartas(MatrizIda,Recibido,MatrizVuelta);
    (* Cobrar las cartas si hay combinación válida *)
    IF Recibido>2 THEN
      BEGIN
        (* Eliminar las cartas cobradas *)
        EliminarPintarCartas(MDC,MatrizVuelta,Recibido);
        (* Cobrar los ejércitos *)
        FOR pj:=1 TO d DIV 3 DO
          BEGIN
            Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar);
            CASE Juego.Modalidad OF
              1: BEGIN
                   Inc(Juego.NumSold,5);
                   Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,5)
                 END;
              2: BEGIN
                   Inc(Juego.NumSold,3+Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar);
                   Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,
                       3+Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)
                 END;
              3: IF Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar<10
                   THEN BEGIN
                          Inc(Juego.NumSold,3+TRUNC(Elev(1.6,Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)));
                          Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,
                              3+TRUNC(Elev(1.6,Juego.Jugador[Juego.Poles[Juego.Actual]].CobrarCar)))
                        END
                   ELSE BEGIN
                          Inc(Juego.NumSold,3+TRUNC(Elev(1.6,9)));
                          Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,3+TRUNC(Elev(1.6,9)))
                        END
            END (* Case *)
          END (* For j a múltiplo de 3 *)
      END; (* IF recibido>2 *)
    (* Comprobar saturación de ejercitos *)
    NPaises:=0;NEjercitos:=0;
    FOR pj:=1 TO 42 DO IF Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual] THEN Inc(NPaises);
    FOR pj:=1 TO 42 DO IF Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual]
      THEN Inc(NEjercitos,Juego.Pais[pj].Ejercitos);
    IF NEjercitos+Juego.NumSold>NPaises*99 THEN Juego.NumSold:=(99*NPaises)-NEjercitos;
    (* Activar estados según sea necesario *)
    IF Juego.NumSold>0 THEN
      BEGIN
        PintarSoldados(Juego.NumSold);
        DibujarTurnos;
        Juego.Estado:=Repartiendo
      END
END; (* CCOrdenador *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE LlamarCartas;
  VAR
    Proc      : TFarProc;
    pj        : byte;
    Resultado : integer;
  BEGIN
    Proc:=MakeProcInstance(@ProcesarCartas, Instancia);
    Resultado:=DialogBox(Instancia,'Cartas', Ventana, Proc);
    FreeProcInstance(Proc);
    ValidateRect(Ventana,NIL);
    ShowWindow(Carta,SW_ShowNA);
    IF Resultado=1 THEN
      BEGIN
        DibujarTurnos;
        FOR pj:=1 TO 42 DO IF Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual] THEN PintarPais(pj,Negro);
        Juego.Estado:=Repartiendo;
        PintarSoldados(Juego.NumSold)
      END
  END; (* LlamarCartas *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE LlamarVictoria;
  VAR
    Proc : TFarProc;
  BEGIN
    PintarPais(Juego.PAtaca,Negro);
    Proc:=MakeProcInstance(@Victoria, Instancia);
    DialogBox(Instancia,'Victoria', Ventana, Proc);
    FreeProcInstance(Proc);
    IniDos;
    EnableMenuItem(GetMenu(Ventana),103,MF_Grayed);
    EnableMenuItem(GetMenu(Ventana),104,MF_Grayed);
    EnableMenuItem(GetMenu(Ventana),106,MF_Grayed);
    EnableMenuItem(GetMenu(Ventana),107,MF_Grayed);
    EnableWindow(Hecho,False);
    EnableWindow(Fortificar,False);
    EnableWindow(Carta,False);
    Juego.Activo:=False;
    Juego.Estado:=Ninguno
  END; (* LlamarVictoria *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE ProcesoCirculando(PLugar:byte);
  BEGIN
    IF Juego.Pais[PLugar].Amo=0 THEN
      BEGIN
        Juego.Pais[PLugar].Amo:=Juego.Poles[Juego.Actual];
        PintarPais(PLugar,Negro);
        Inc(Juego.i);
        MoverTurno;
        DibujarTurnos
      END;
    IF Juego.i=43 THEN (* ya que se incrementa el contador después de *)
      BEGIN
        Juego.i:=1; (* Ahora contar las vueltas de iniciando *)
        Juego.Actual:=1;
        DibujarTurnos;
        Juego.NumSold:=5; (* nº soldados primera vuelta del reparto inicial soldados *)
        Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,Juego.NumSold);
        PintarSoldados(Juego.NumSold);
        Juego.Estado:=Iniciando (* Activación del siguiente estado *)
      END
  END; (* ProcesoCirculando *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE ProcesoIniciando(PLugar:byte;PSoldados:byte);
  BEGIN
    IF Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual] THEN
      BEGIN
        (* Añadirlos soldados al pais *)
        Inc(Juego.Pais[PLugar].Ejercitos,PSoldados);
        Juego.NumSold:=Juego.NumSold-PSoldados;
        (* Escribir en el mapa el nº de soldados *)
        PintarPais(PLugar,Negro);
        (* Mirar si ya termino de repartir el jugador *)
        IF Juego.NumSold<=0
          THEN
            BEGIN
              MoverTurno;
              IF Juego.Actual=1 THEN Inc(Juego.i);
              CASE Juego.i OF
                1 : Juego.NumSold:=5;
                2 : IF Juego.Poles[6]<>0
                      THEN Juego.NumSold:=4
                      ELSE Juego.NumSold:=5;
                3 : IF Juego.Poles[6]<>0
                      THEN Juego.NumSold:=2
                      ELSE IF Juego.Poles[5]<>0
                             THEN Juego.NumSold:=3
                             ELSE Juego.NumSold:=5;
                4 : BEGIN
                      IF Juego.Poles[2]<>0 THEN Juego.NumSold:=2;
                      IF Juego.Poles[3]<>0 THEN Juego.NumSold:=3;
                      IF Juego.Poles[4]<>0 THEN Juego.NumSold:=2;
                      IF Juego.Poles[5]<>0 THEN Juego.NumSold:=2;
                      IF Juego.Poles[6]<>0 THEN Juego.NumSold:=1;
                    END;
                5 : BEGIN
                      IF Juego.Poles[2]<>0 THEN Juego.NumSold:=1;
                      IF Juego.Poles[3]<>0 THEN Juego.NumSold:=2;
                      IF Juego.Poles[4]<>0 THEN Juego.NumSold:=1;
                      IF Juego.Poles[5]<>0 THEN Juego.NumSold:=1;
                      IF Juego.Poles[6]<>0 THEN Juego.NumSold:=1;
                    END;
                6 : BEGIN
                      IF Juego.Poles[5]<>0
                        THEN
                          BEGIN (* Finalización del reparto *)
                            EnableMenuItem(GetMenu(Ventana),106,MF_Enabled);
                            (* Datos de uso para el primer jugador en 1º turno *)
                            Juego.NumSold:=TotalCobrar(Juego.Poles[Juego.Actual(*=1*)]);
                            (* Cambio al siguiente estado *)
                            Juego.Estado:=Repartiendo
                          END
                        ELSE
                          Juego.NumSold:=1
                    END;
                7 : BEGIN (* Si se llega aquí -> fin definitivo de reparto *)
                      EnableMenuItem(GetMenu(Ventana),106,MF_Enabled);
                      (* Datos de uso para el primer jugador en 1º turno *)
                      Juego.NumSold:=TotalCobrar(Juego.Poles[Juego.Actual(*=1*)]);
                      (* Cambio al siguiente estado *)
                      Juego.Estado:=Repartiendo
                    END
              END; (* Case *)
              Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,Juego.NumSold);
              PintarSoldados(Juego.NumSold);
              DibujarTurnos
            END
          ELSE (* Juego.NumSold<=0 *)
            PintarSoldados(Juego.NumSold)
      END (* IF Amo=Actual *)
  END; (* ProcesoIniciando *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION ProcesoRepartiendo(PLugar,PSoldados:byte):boolean; (* True si éxito *)
  VAR
    MemDC : HDC;
  BEGIN (* ProcesoRepartiendo *)
    ProcesoRepartiendo:=True;
    IF Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual] THEN
      BEGIN
        (* Añadirlos soldados al pais *)
        IF Juego.Pais[PLugar].Ejercitos+PSoldados>99
          THEN ProcesoRepartiendo:=False
          ELSE
            BEGIN
              Inc(Juego.Pais[PLugar].Ejercitos,PSoldados);
              Juego.NumSold:=Juego.NumSold-PSoldados;
              (* Escribir en el mapa el nº de soldados *)
              PintarPais(PLugar,Negro)
            END;
        (* Comprobar finalización *)
        IF Juego.NumSold<=0
          THEN  (* Se terminó con el jugador correspondiente *)
            BEGIN
              (* Se escoge el utlimo pais usado *)
              Juego.PAtaca:=PLugar;
              PintarPais(Juego.PAtaca,Borde);
              (* incrementar el nº total de soldados del jugador *)
              Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,
                  TotalCobrar(Juego.Poles[Juego.Actual]) );
              (* Activar los menús y botones necesarios *)
              EnableWindow(Hecho,True);
              EnableWindow(Fortificar,True);
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0
                THEN EnableWindow(Carta,True)
                ELSE EnableWindow(Carta,False);
              (* Poner los créditos *)
              MemDC:=CreateCompatibleDC(MDC);
              SelectObject(MemDC,Creditos);
              BitBlt(MDC,211,379,420,55,MemDC,0,0,SRCCopy);
              DeleteDC(MemDC);
              (* Cambiar al estado *)
              Juego.Estado:=Atacando
            END
          ELSE (* Juego.NumSold<=0 *)
            PintarSoldados(Juego.NumSold)
      END (* IF Pais.Amo=Actual *)
  END; (* ProcesoRepartiendo *)
(**************************************************************************************)
(**************************************************************************************)
(* Añade a bolsa perdidos: ppais *)
PROCEDURE LlenarPerdidos(PPais:byte);
  VAR
    pj           : byte;
    HayQueAnadir : boolean;
  BEGIN (* LlenarPerdidos *)
    WITH Juego.Jugador[Juego.Pais[Ppais].Amo] DO
      BEGIN
        {HayQueAnadir:=True;
        FOR pj:=1 TO 10 DO IF ElPerdido[pj]=PPais THEN HayQueAnadir:=False;
        IF HayQueAnadir THEN
          BEGIN}
            IF Perdidos=10
              THEN FOR pj:=1 TO 9 DO ElPerdido[pj]:=ElPerdido[pj+1]
              ELSE Inc(Perdidos);
            ElPerdido[Perdidos]:=PPais
         { END (* If hay que añadir *)}
      END; (* WITH *)
  END; (* LlenarPerdidos *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION ProcesoAtacando(PLugar:byte):byte;
  VAR
    EsFront   : boolean;
    pm,Salida : byte;
  BEGIN
    Salida:=0; (* Será la solución de la función *)
    IF Juego.PAtaca=0
      THEN
        BEGIN (* Se está seleccionando un pais que atacará *)
          IF ((Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual])AND(Juego.Pais[PLugar].Ejercitos>0)) THEN
            BEGIN
              Juego.PAtaca:=PLugar;
              PintarPais(Juego.PAtaca,Borde)
            END
        END
      ELSE  (* else de Pataca=0 *)
        BEGIN
          IF Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual]
            THEN (* Se está señalando otro pais para atacar *)
              BEGIN IF Juego.Pais[PLugar].Ejercitos<>0 THEN
                BEGIN
                  PintarPais(Juego.PAtaca,Negro);
                  Juego.PAtaca:=PLugar;
                  PintarPais(Juego.PAtaca,Borde)
                END
              END
            ELSE (* Si el pais es fronterizo se resuelve el ataque *)
              BEGIN
                EsFront:=False;
                FOR pm:=1 TO DatPais[Juego.PAtaca].Fronteras DO
                  IF PLugar=DatPais[Juego.PAtaca].Frontera[pm]THEN EsFront:=True;
                IF EsFront THEN
                  BEGIN
                    Juego.Batalla[1]:=Juego.PAtaca;
                    Juego.Batalla[2]:=PLugar;
                    (* RESOLUCION DEL ATAQUE *)
                    IF EnemigosQueSobreviven(Juego.Batalla[1],Juego.Batalla[2])<0
                      THEN (* Se ha ganado el pais *)
                        BEGIN
                          Juego.Invadio:=True;
                          Inc(Juego.Jugador[Juego.Pais[Juego.Batalla[2]].Amo].Derrotas);
                          Inc(Juego.Jugador[Juego.Pais[Juego.Batalla[1]].Amo].Victorias);
                          (* Verificar muerte del contrincante *)
                          AuxS:='El jugador '+Juego.Jugador[Juego.Pais[Juego.Batalla[2]].Amo].Nombre+' ha muerto.';
                          AuxS[Length(AuxS)+1]:=Chr(0);
                          IF EliminarSiMuerto(Juego.Pais[Juego.Batalla[2]].Amo)
                            THEN Salida:=2 (* oponente ha muerto *)
                            ELSE BEGIN
                                   LlenarPerdidos(Juego.Batalla[2]); (* Almacena pais perdido *)
                                   Juego.Pais[Juego.Batalla[2]].Amo:=Juego.Poles[Juego.Actual];
                                   PintarPais(Juego.Batalla[2],Negro)
                                 END;
                          (* Comprobación de victoria *)
                          IF Juego.Poles[2]=0
                            THEN Salida:=1 (* Jugador ha ganado *)
                            ELSE
                              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>4
                                THEN Inc(Salida,3); (* Se debe cobrar cartas además *)
                          (* Colocación de soldados según modalidad *)
                          ColocarSegunModalidad(Juego.Batalla[1],Juego.Batalla[2])
                        END (* se ganó el pais *)
                      ELSE
                        BEGIN (* no se ha ganado el pais *)
                          PintarPais(Juego.Batalla[2],Negro);
                          IF Juego.Pais[Juego.Batalla[1]].Ejercitos>0
                            THEN PintarPais(Juego.Batalla[1],Borde)
                            ELSE BEGIN
                                   Juego.PAtaca:=0;
                                   PintarPais(Juego.Batalla[1],Negro)
                                 END
                        END (* No se ganó el pais *)
                    (* REMATE DE LA RESOLUCION DEL ATAQUE *)
                  END (* If EsFront *)
              END (* Else del if atacando *)
        END; (* Else if PAtaca=0 *)
    ProcesoAtacando:=Salida
  END;  (* ProcesoAtacando *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE ProcesoGanando(PLugar,PSoldados:byte);
  VAR
    MemDC : HDC;
  BEGIN
    IF ((PLugar=Juego.Batalla[1])OR(PLugar=Juego.Batalla[2])) THEN
      BEGIN
        (* Añadir los soldados al pais *)
        Inc(Juego.Pais[PLugar].Ejercitos,PSoldados);
        Juego.NumSold:=Juego.NumSold-PSoldados;
        (* Escribir en el mapa el nº de soldados *)
        PintarPais(PLugar,Negro);
        (* Comprobar finalización *)
        IF Juego.NumSold<=0
          THEN
            BEGIN
              Juego.PAtaca:=PLugar; (* Se escoge el utlimo pais usado *)
              PintarPais(Juego.Batalla[1],Negro);
              PintarPais(Juego.Batalla[2],Negro);
              PintarPais(Juego.PAtaca,Borde);
              (* Activar los menús y botones necesarios *)
              EnableWindow(Hecho,True);
              EnableWindow(Fortificar,True);
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0
                THEN EnableWindow(Carta,True)
                ELSE EnableWindow(Carta,False);
              (* Poner los créditos *)
              MemDC:=CreateCompatibleDC(MDC);
              SelectObject(MemDC,Creditos);
              BitBlt(MDC,211,379,420,55,MemDC,0,0,SRCCopy);
              DeleteDC(MemDC);
              (* Cambiar al estado *)
              Juego.Estado:=Atacando
            END
          ELSE (* Juego.NumSold<=0 *)
            PintarSoldados(Juego.NumSold)
      END (* IF Lugar in batalla *)
  END; (* Ganando *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION ProcesoFortificando(PLugar,PSoldados:byte):boolean;
  VAR
    MemDC   : HDC;
    pm      : byte;
    EsFront : boolean;
  BEGIN
    ProcesoFortificando:=True;
    IF Juego.PFortifica=0
      THEN
        BEGIN
          IF ( (Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual])AND
               (Juego.Pais[PLugar].Ejercitos>0) ) THEN
            BEGIN
              Juego.PFortifica:=PLugar;
              Juego.NumSold:=Juego.Pais[Juego.PFortifica].Ejercitos;
              Juego.Pais[Juego.PFortifica].Ejercitos:=0;
              PintarPais(Juego.PFortifica,Borde);
              PintarSoldados(Juego.NumSold)
            END
        END
      ELSE (* Si ya se está fortificando un país *)
        BEGIN
          EsFront:=False;
          FOR pm:=1 TO DatPais[Juego.PFortifica].Fronteras DO
            IF ( ((PLugar=DatPais[Juego.PFortifica].Frontera[pm]) AND
                 (Juego.Pais[PLugar].Amo=Juego.Poles[Juego.Actual]))
                 OR (PLugar=Juego.PFortifica) )
               THEN EsFront:=True;
          IF EsFront (* se ha seleccionado un pais destino válido *)
            THEN
              BEGIN
                (* Comprobar que caben *)
                pm:=Juego.Pais[PLugar].Ejercitos+PSoldados;
                IF Juego.Pais[PLugar].Fortif>0 THEN Inc(pm,Juego.Pais[PLugar].Fortif);
                IF pm>99
                  THEN ProcesoFortificando:=False
                  ELSE
                    BEGIN
                      (* Quitarlos del pais base *)
                      Dec(Juego.NumSold,PSoldados);
                      (* Añadirlos al nuevo Pais *)
                      IF ((NOT Juego.Pais[PLugar].Recibido)AND(Juego.Pais[PLugar].Fortif=0))
                        THEN  (* Añadir un soldado que se podrá mover *)
                          BEGIN
                            Inc(Juego.Pais[PLugar].Ejercitos);
                            Dec(PSoldados);
                            Juego.Pais[PLugar].Recibido:=True
                          END;
                      IF PSoldados=0
                        THEN Juego.Pais[PLugar].Fortif:=-1
                        ELSE
                          BEGIN
                            IF Juego.Pais[PLugar].Fortif=-1 THEN Inc(PSoldados);
                            Inc(Juego.Pais[PLugar].Fortif,PSoldados)
                          END;
                      (* repintar los paises modificados *)
                      PintarPais(PLugar,Negro)
                    END;
                IF Juego.NumSold<=0
                  THEN
                    BEGIN
                      PintarPais(Juego.PFortifica,Negro);
                      Juego.PFortifica:=0;
                      SelectObject(MemDC, Creditos);
                      MemDC:=CreateCompatibleDC(MDC);
                      SelectObject(MemDC,Creditos);
                      BitBlt(MDC,211,379,420,55,MemDC,0,0,SRCCopy);
                      DeleteDC(MemDC);
                    END
                  ELSE
                    BEGIN
                      PintarSoldados(Juego.NumSold);
                      PintarPais(Juego.PFortifica,Borde)
                    END (* Else *)
              END
        END (* Else del if Juego.PFortifica=0 *)
  END; (* Fortificando *)
(**************************************************************************************)
(**************************************************************************************)
(* Llena la bolsa con los paises actuales propios *)
PROCEDURE CalculoDePaises;
  VAR m :byte;
  BEGIN (* CalculoDePaises *)
    WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO
      BEGIN
        FinBolsa:=0;
        FOR m:=1 TO 42 DO
          IF Juego.Pais[m].Amo=Juego.Poles[Juego.Actual] THEN
            BEGIN
              Inc(FinBolsa);
              BolsaPais[FinBolsa]:=m
            END
      END (* WITH *)
  END; (* CalculoDePaises *)
(**************************************************************************************)
(*****************************************************************************************************)
PROCEDURE HallarCamino(PaisInicio,PaisDestino:byte;VAR PCamino,PCaminoSol:TCamino;Paso:byte;VAR Maximo:byte);
  VAR
    pj,pm : byte;
  FUNCTION NoEsta(PPais:byte):boolean;
    VAR
      px : byte;
    BEGIN
      NoEsta:=True;
      FOR px:=0 TO 10 DO IF PCamino[px]=PPais THEN NoEsta:=False
    END; (* NoEsta *)

   BEGIN (* HallarCamino *)
    PCamino[Paso]:=PaisInicio;
    IF PaisInicio=PaisDestino
      THEN
        BEGIN (* reducir el máximo y limpiar el resto del camino *)
          Maximo:=Paso-1;
          FOR pm:=Paso+1 TO 10 DO PCamino[pm]:=0;
          PCaminoSol:=PCamino
        END (* El pais es el destino *)
      ELSE IF Paso<Maximo THEN FOR pj:=1 TO DatPais[PaisInicio].Fronteras DO
        IF NoEsta(DatPais[PaisInicio].Frontera[pj])
          THEN
            BEGIN
              HallarCamino(DatPais[PaisInicio].Frontera[pj],PaisDestino,PCamino,PCaminoSol,Paso+1,Maximo);
              FOR pm:=Paso+1 TO 10 DO PCamino[pm]:=0
            END
  END; (* HallarCamino *)
(*****************************************************************************************************)
(**************************************************************************************)
PROCEDURE BuscaEn(PContinente:byte;VAR PPaisInicio:byte);
  VAR
    pl : byte;
  BEGIN
    CASE PContinente OF
      1 : (* América del Sur *)
          FOR pl:=1 TO 4 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl;
      2 : (* América del Norte *)
          FOR pl:=5 TO 13 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl;
      3 : (* Europa *)
          FOR pl:=14 TO 20 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl;
      4 : (* África *)
          FOR pl:=21 TO 26 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl;
      5 : (* Asia *)
          FOR pl:=27 TO 38 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl;
      6 : (* Oceanía *)
          FOR pl:=39 TO 42 DO IF Juego.Pais[pl].Amo=Juego.Poles[Juego.Actual] THEN
            IF ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
               Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-PPaisInicio)
                 >ABS(Juego.Jugador[Juego.Poles[Juego.Actual]].ElPerdido[
                   Juego.Jugador[Juego.Poles[Juego.Actual]].Perdidos ]-pl)
                     THEN PPaisInicio:=pl
    END (* Case *)
  END; (* BuscaEN *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION EscogerPaisFronterizo(PPais:byte):byte;
  VAR
    pj,Aux : byte;
  BEGIN
    Aux:=0;
    FOR pj:=1 TO DatPais[PPais].Fronteras DO
      IF Juego.Pais[DatPais[PPais].Frontera[pj]].Amo=Juego.Poles[Juego.Actual]
        THEN
          BEGIN
            IF Aux=0 THEN Aux:=DatPais[PPais].Frontera[pj]
                     ELSE BEGIN
                            IF Juego.Pais[pj].Ejercitos>Juego.Pais[Aux].Ejercitos
                              THEN Aux:=DatPais[PPais].Frontera[pj]
                          END
          END; (* IF la frontera es del jugador actual *)
    EscogerPaisFronterizo:=Aux
  END; (* EscogerPaisFronterizo *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE HallarMejorCamino(PPaisFinal:byte; VAR PElCamino:TCamino);
  VAR
    CaminoProvi       : TCamino;
    PaisInicio,Maximo : byte;
    OldCursor         : HCursor;
    pc,pv             : byte;
  BEGIN
    OldCursor:=SetCursor(LoadCursor(0,IDC_Wait));
    PElCamino[0]:=EscogerPaisFronterizo(PPaisFinal);
    IF PElCamino[0]=0
      THEN
        BEGIN
          PaisInicio:=100;
          CASE PPaisFinal OF
             1..4  : BEGIN (* América Sur *)
                       BuscaEn(1,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(6,PaisInicio)
                     END; (* América Sur *)
             5..13 : BEGIN (* América Norte *)
                       BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(1,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(6,PaisInicio)
                     END; (* América Norte *)
            14..20 : BEGIN (* Europa *)
                       BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(1,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(6,PaisInicio)
                     END; (* Europa *)
            21..26 : BEGIN (* Africa *)
                       BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(1,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(6,PaisInicio)
                     END; (* Africa *)
            27..38 : BEGIN (* Asia *)
                       BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(6,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(1,PaisInicio)
                     END; (* Asia *)
            39..42 : BEGIN (* Oceanía *)
                       BuscaEn(6,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(5,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(3,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(2,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(4,PaisInicio);
                       IF PaisInicio=100 THEN BuscaEn(1,PaisInicio)
                     END  (* Oceanía *)
          END; (* Case *)
          IF PaisInicio=100 THEN MessageBox(Ventana,'ESTO NO FUNCIONA','ERROR',MB_IconAsterisk);
          Maximo:=10;
          HallarCamino(PaisInicio,PPaisFinal,CaminoProvi,PElCamino,0,Maximo);
          (* Comprobación de que no hay paises del jugador actual en el camino *)
          pc:=1;
          WHILE pc<11 DO
            BEGIN
              IF PElCamino[pc]<>0 THEN
                IF Juego.Pais[PElCamino[pc]].Amo=Juego.Poles[Juego.Actual]
                  THEN
                    BEGIN
                      FOR pv:=0 TO 10-pc DO PElCamino[pv]:=PElCamino[pc+pv];
                      pc:=0
                    END;
              Inc(pc)
            END (* While *)
        END (* IF no hay paises en la frontera *)
      ELSE
        BEGIN
          Juego.Camino[1]:=PPaisFinal;
          FOR pv:=2 TO 10 DO Juego.Camino[pv]:=0
        END;
    (* Devolvemos el cursor *)
    SetCursor(OldCursor)
  END; (* HallarMejorCamino *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION SonVecinos(PPaisUno,PPaisDos:byte):boolean;
  VAR
    pj : byte;
  BEGIN
    SonVecinos:=False;
    FOR pj:=1 TO DatPais[PPaisUno].Fronteras DO
      IF DatPais[PPaisUno].Frontera[pj]=PPaisDos THEN SonVecinos:=True
  END; (* SonVecinos *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION TengoEnemigosEnLasFronteras(PPais:byte):boolean;
  VAR
    pj : byte;
  BEGIN
    TengoEnemigosEnLasFronteras:=False;
    FOR pj:=1 TO DatPais[PPais].Fronteras DO
      IF Juego.Pais[DatPais[PPais].Frontera[pj]].Amo<>Juego.Poles[Juego.Actual]
        THEN TengoEnemigosEnLasFronteras:=True
  END; (* TengoEnemigosEnLasFronteras *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION EsMio(PContinente:byte):boolean;
  VAR
    pj : byte;
  BEGIN
    IF ((PContinente>0)AND(PContinente<7))
      THEN
        BEGIN
          EsMio:=True;
          FOR pj:=C[PContinente].X TO C[PContinente].Y DO
            IF Juego.Pais[pj].Amo<>Juego.Poles[Juego.Actual] THEN EsMio:=False
        END
      ELSE EsMio:=False
  END; (* EsMio *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION QuedaSitioEn(PContinente:byte):boolean;
  VAR
    pj : byte;
  BEGIN
    IF PContinente>0
      THEN
        BEGIN
          QuedaSitioEn:=False;
          FOR pj:=C[PContinente].X TO C[PContinente].Y DO
            IF Juego.Pais[pj].Amo=0 THEN QuedaSitioEn:=True
        END
      ELSE QuedaSitioEn:=False
  END; (* QuedaSitioEn *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION TengoSitioEn(PContinente:byte):boolean;
  VAR
    pj : byte;
  BEGIN
    IF PContinente>0
      THEN
        BEGIN
          TengoSitioEn:=False;
          FOR pj:=C[PContinente].X TO C[PContinente].Y DO
            IF Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual] THEN TengoSitioEn:=True
        END
      ELSE TengoSitioEn:=False
  END; (* TengoSitioEn *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION TengoQuePonerEn(PContinente:byte):boolean;
  VAR
    pj,UnAmo: byte;
  BEGIN
    IF PContinente>0
      THEN
        BEGIN
          UnAmo:=0;
          FOR pj:=C[PContinente].X TO C[PContinente].Y DO
            IF Juego.Pais[pj].Amo<>0 THEN UnAmo:=Juego.Pais[pj].Amo;
          TengoQuePonerEn:=True;
          FOR pj:=C[PContinente].X TO C[PContinente].Y DO
            IF ( (Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual])
                 OR ((Juego.Pais[pj].Amo<>UnAmo)AND(Juego.Pais[pj].Amo<>0)) ) THEN TengoQuePonerEn:=False
        END
      ELSE TengoQuePonerEn:=False
  END; (* TengoQuePonerEn *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION CuantosCaben(PContinente:byte):byte;
  VAR
    pj  : byte;
    Aux : integer;
  BEGIN
    Aux:=0;
    IF ((PContinente>0)AND(PContinente<7)) THEN
      FOR pj:=C[PContinente].X TO C[PContinente].Y DO
        IF Juego.Pais[pj].Amo=Juego.Poles[Juego.Actual]
          THEN Aux:=Aux+99-Juego.Pais[pj].Ejercitos;
    IF Aux>255 THEN CuantosCaben:=255
               ELSE CuantosCaben:=Aux
  END; (* CuantosCaben *)
(**************************************************************************************)
(**************************************************************************************)
FUNCTION HallarContinente:byte;
  TYPE
    TConti = RECORD
               Paises : byte;
              { Tuyos  : byte;}
               Ejer   : integer
             END;
  VAR
    Continentes : Array[1..6] OF TConti;
    hi          : Byte;
    Media       : Real;

  PROCEDURE HallarDatosContinente(r,t:byte;VAR PConti: TConti);
    VAR
      hhi :byte;
    BEGIN
      PConti.Paises:=0;
      PConti.Ejer  :=0;
      FOR hhi:=r TO t DO
        IF Juego.Pais[hhi].Amo<>Juego.Poles[Juego.Actual]
          THEN
            BEGIN
              Inc(PConti.Ejer,Juego.Pais[hhi].Ejercitos);
              Inc(PConti.Paises)
            END
    END; (* HallarDatosContinente *)

  BEGIN (* HallarContinente *)
    HallarDatosContinente( 1, 4,Continentes[1]);
    HallarDatosContinente( 5,13,Continentes[2]);
    HallarDatosContinente(14,20,Continentes[3]);
    HallarDatosContinente(21,26,Continentes[4]);
    HallarDatosContinente(27,38,Continentes[5]);
    HallarDatosContinente(39,42,Continentes[6]);
    (* escoger continente con la menor media *)
    WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO FOR hi:=1 TO 6 DO
      IF EsMio(ElCont[hi]) THEN HallarContinente:=ElCont[hi];
    Media:=100;
    FOR hi:=1 TO 6 DO IF ((Continentes[hi].Paises>0)
                          AND(Continentes[hi].Ejer/Continentes[hi].Paises<Media)
                          AND(TengoSitioEn(hi)) )
      THEN
        BEGIN
          HallarContinente:=hi;
          Media:=Continentes[hi].Ejer/Continentes[hi].Paises
        END
  END; (* HallarContinente *)
(**************************************************************************************)
(**************************************************************************************)
PROCEDURE MeterContinente;
  VAR
    pj           : byte;
    HayQueAnadir : boolean;
  BEGIN
    WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO
      BEGIN
        HayQueAnadir:=True;
        FOR pj:=1 TO Conts DO IF ElCont[pj]=Continente THEN HayQueAnadir:=False;
        IF HayQueAnadir THEN
          BEGIN
            Inc(Conts);
            ElCont[Conts]:=Continente
          END
      END (* With *)
  END; (* MeterContinente *)
(**************************************************************************************)








(**************************************************************************************)
(* Funcion 'callback' que maneja la ventana principal del juego *)
FUNCTION VentanaProc(PVentana:HWnd; PMensaje,PWParam:Word; PlParam: LongInt): LongInt; EXPORT;
  VAR
    Proceso  : TFarProc;
    RLimites : TRect;
    Estruct  : TPaintStruct;
    MemDC    : HDC;
    Lugar    : byte; (* País que se está apuntando *)
    pj,pl    : byte;

  BEGIN (* VentanaProc *)
    VentanaProc := 0;  (* Por defecto se supone que se procesa el mensaje *)
    (* Control del movimiento de la ventana *)
    IF NOT IsIconic(PVentana) THEN EnableMenuItem(GetSystemMenu(Ventana,False),SC_Move,MF_Grayed);
    (* Acción según el mensaje *)
    CASE PMensaje OF
      WM_LButtonDown:
        IF ((Juego.Poles[Juego.Actual]<>0)AND(Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1)) THEN BEGIN
          Inicio.X:=LoWord(PLParam); Inicio.Y:=HiWord(PLParam);
          IF NOT(Juego.Estado IN [Circulando,Atacando]) THEN
          IF ((Inicio.X>210) AND (Inicio.X<631) AND (Inicio.Y>378) AND (Inicio.Y<434)) THEN
            BEGIN
              SelectObject(MDC, GetStockObject(Null_Brush));
              Juego.Seleccion:=True;
              Fin:=Inicio;
              (* eliminar la selección anterior *)
              PintarMonecos;
              (* Limitar el movimiento del Cursor *)
              SetCapture(PVentana);
              GetWindowRect(PVentana,RLimites);
              RLimites.Left:=RLimites.Left+212; RLimites.Top:=RLimites.Top+418;
              RLimites.Right:=RLimites.Left+420; RLimites.Bottom:=RLimites.Top+55;
              ClipCursor(@RLimites);
              (* Invertir pixels bajo el lapiz *)
              SetROP2(MDC, r2_Not);
              (* dibujar el rectangulo de selección *)
              Rectangle(MDC,Inicio.X,Inicio.Y,Fin.X,Fin.Y);
            END (* If *)
        END; (* WM_LButtonDown *)
      WM_MouseMove:
        BEGIN
          IF Juego.Seleccion THEN
            BEGIN
              Rectangle(MDC,Inicio.X,Inicio.Y,Fin.X,Fin.Y); (* Borrar el anterior *)
              Fin.X:=LoWord(PLParam); Fin.Y:=HiWord(PLParam); (* Nuevas coordenadas *)
              Rectangle(MDC,Inicio.X,Inicio.Y,Fin.X,Fin.Y) (* Poner el nuevo *)
            END
        END;
      WM_LButtonUp:
        BEGIN
          IF Juego.Seleccion THEN
            BEGIN
              Rectangle(MDC,Inicio.X,Inicio.Y,Fin.X,Fin.Y); (* Borrar el último *)
              SetROP2(MDC,R2_CopyPen);
              SelectObject(MDC, GetStockObject(White_Brush));
              (* Liberar la limitación del movimiento del cursor *)
              ReleaseCapture;
              ClipCursor(nil);
              (* Fin de la operación de selección *)
              Juego.Seleccion:=False;
              (* Averiguar que soldados se han seleccionado *)
              Juego.Pri:=((Inicio.X-211)DIV 42)+1;
              Juego.Ult:=((Fin.X-211)DIV 42)+1;
              IndicarSoldados(Juego.Pri,Juego.Ult) (* Invierte soldados escogidos *)
            END; (* IF Seleccion *)
          (* PARTE PRINCIPAL DEL RISK *)
          (* Averiguar donde se está pisando *)
          Fin.X:=LoWord(PLParam);
          Fin.Y:=HiWord(PLParam);
          Lugar:=QuePais(Fin.X,Fin.Y);
          (* Contemplación según el caso *)
          IF ((Juego.Poles[Juego.Actual]<>0)AND(Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1)) THEN
          CASE Juego.Estado OF
            Circulando   : IF Lugar<>0 THEN ProcesoCirculando(Lugar);
            Iniciando    : IF Lugar<>0 THEN ProcesoIniciando(Lugar,TotalSoldados(Juego.Pri,Juego.Ult));
            Repartiendo  : IF Lugar<>0 THEN IF NOT ProcesoRepartiendo(Lugar,TotalSoldados(Juego.Pri,Juego.Ult)) THEN
                             MessageBox(Ventana,'Un pais no puede tener más de 99 ejercitos.','Aviso',MB_IconStop OR MB_OK);
            Atacando     : IF Lugar<>0 THEN
                             CASE ProcesoAtacando(Lugar) OF
                               1 : LlamarVictoria;
                               2 : MessageBox(Ventana,@AuxS[1],'Aviso',MB_IconAsterisk OR MB_OK);
                               3 : BEGIN
                                     Juego.Estado:=Repartiendo;
                                     LlamarCartas
                                   END;
                               5 : BEGIN
                                     MessageBox(Ventana,@AuxS[1],'Aviso',MB_IconAsterisk OR MB_OK);
                                     Juego.Estado:=Repartiendo;
                                     LlamarCartas
                                   END
                             END; (* Case y Atacando *)
            Ganando      : ProcesoGanando(Lugar,TotalSoldados(Juego.Pri,Juego.Ult));
            Fortificando : IF Lugar<>0 THEN IF NOT ProcesoFortificando(Lugar,TotalSoldados(Juego.Pri,Juego.Ult)) THEN
                             MessageBox(Ventana,'Un pais no puede tener más de 99 ejercitos.','Aviso',MB_IconStop OR MB_OK);
          END (* Case estado of *)
        END; (* WM_LButtonUp *)
      WM_Command:
        CASE PWparam OF
          101: (* Partida_Nueva *)
            BEGIN (* 101 *)
              IF Juego.Activo THEN
                IF MessageBox(PVentana,'Si empieza una partida nueva perderá la actual,'+Chr(13)+
                                       '                         ¿Continuar?','Aviso',MB_IconStop OR MB_YesNo)
                   = IDNo THEN Exit
                          ELSE (* Aquí se reinicializan todo como si fuera el principio *)
                            BEGIN
                              IniDos;
                              EnableMenuItem(GetMenu(PVentana),103,MF_Grayed);
                              EnableMenuItem(GetMenu(PVentana),104,MF_Grayed);
                              EnableMenuItem(GetMenu(PVentana),106,MF_Grayed);
                              EnableMenuItem(GetMenu(PVentana),107,MF_Grayed);
                              EnableWindow(Hecho,False);
                              EnableWindow(Fortificar,False);
                              EnableWindow(Carta,False);
                              Juego.Activo:=False;
                              Juego.Estado:=Ninguno
                            END;
              Repintar(MDC);
              Proceso := MakeProcInstance(@PartidaNueva, Instancia);
              DialogBox(Instancia,'PartidaNueva', PVentana, Proceso);
              FreeProcInstance(Proceso);
              ValidateRect(PVentana,Nil);
              IF Juego.Activo THEN IF Aleatorio
                THEN
                  BEGIN (* Aleatorio *)
                    EnableMenuItem(GetMenu(PVentana),103,MF_Enabled); (* Salvar *)
                    EnableMenuItem(GetMenu(PVentana),104,MF_Enabled); (* SalvarComo *)
                    EnableMenuItem(GetMenu(PVentana),106,MF_Enabled); (* OpcionesCombate *)
                    EnableMenuItem(GetMenu(PVentana),107,MF_Enabled); (* Estadísticas *)
                    RepartirTerritorios; (* Reparte aleatorioamente paises *)
                    DibujarTurnos;
                    Juego.NumSold:=5; (* Soldados iniciales para iniciando *)
                    Inc(Juego.Jugador[Juego.Poles[Juego.Actual]].Total,Juego.NumSold);
                    PintarSoldados(Juego.NumSold);
                    Juego.i:=1; (* Nº de vuelta en el estado iniciando *)
                    Juego.Estado:=Iniciando (* Reparto soldados iniciales *)
                  END (* IF *)
                ELSE (* Rotacional *)
                  BEGIN
                    EnableMenuItem(GetMenu(PVentana),103,MF_Enabled); (* Salvar *)
                    EnableMenuItem(GetMenu(PVentana),104,MF_Enabled); (* SalvarComo *)
                    EnableMenuItem(GetMenu(PVentana),106,MF_Enabled); (* OpcionesCombate *)
                    EnableMenuItem(GetMenu(PVentana),107,MF_Enabled); (* Estadísticas *)
                    Juego.i:=1; (* Nº de pais que se está repartiendo *)
                    DibujarTurnos;
                    Juego.Estado:=Circulando
                  END
            END; (* 101 *)
          102: (* Ficheros_Abrir *)
            BEGIN (* 102 *)
              IF Juego.Activo THEN
                IF MessageBox(PVentana,'Si abre una partida nueva perderá la actual,'+Chr(13)+
                                       '                      ¿Continuar?','Aviso',MB_IconStop OR MB_YesNo)
                   = IDNO THEN Exit;
              LStrCpy(@NomFich[1],'*.rsk');
              NomFich[0]:=Char(5);
              Proceso := MakeProcInstance(@AbrirFichero,Instancia);
              IF DialogBox(Instancia,'Cargar', PVentana, Proceso)=1 THEN
                BEGIN
                  (* Restablecer colores *)
                  FOR Pj:=1 TO MaxDibu DO Brocha[Pj].Usada:=False;
                  FOR Pj:=1 TO 6 DO IF Juego.Poles[Pj]<>0
                   THEN Brocha[Juego.Jugador[Juego.Poles[Pj]].Color].Usada:=True;
                  (* Restablecer las cartas usadas *)
                  FOR pj:=1 TO 6 DO IF Juego.Poles[pj]<>0 THEN
                    FOR pl:=1 TO Juego.Jugador[Juego.Poles[pj]].NCartas DO
                      BEGIN
                        Cartas[Juego.Jugador[Juego.Poles[pj]].LaCarta[pl]].Amo:=Juego.Poles[pj];
                        Cartas[Juego.Jugador[Juego.Poles[pj]].Lacarta[pl]].Usada:=True
                      END;
                  (* Activación de menus y botones *)
                  EnableMenuItem(GetMenu(PVentana),103,MF_Enabled);
                  EnableMenuItem(GetMenu(PVentana),104,MF_Enabled);
                  EnableMenuItem(GetMenu(PVentana),106,MF_Enabled);
                  EnableMenuItem(GetMenu(PVentana),107,MF_Enabled);
                  CASE Juego.Estado OF
                    Circulando,Iniciando,Repartiendo,Ganando:
                      BEGIN
                        EnableWindow(Hecho,False);
                        EnableWindow(Fortificar,False)
                      END;
                    Atacando:
                      BEGIN
                        EnableWindow(Hecho,True);
                        EnableWindow(Fortificar,True)
                      END;
                    Fortificando:
                      BEGIN
                        IF Juego.PFortifica=0 THEN EnableWindow(Hecho,True)
                                              ELSE EnableWindow(Hecho,False);
                        EnableWindow(Fortificar,False)
                      END;
                  END; (* Case estado *)
                  IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0
                    THEN EnableWindow(Carta,True)
                    ELSE EnableWindow(Carta,False);
                  Juego.Activo:=True;
                  (* Repintado general *)
                  InvalidateRect(PVentana,NIL,True)
                END;
              FreeProcInstance(Proceso)
            END; (* 102 *)
          103: (* Ficheros_Salvar *)
            BEGIN (* 103 *)
              IF NomFich=''
                THEN SendMessage(PVentana,WM_Command,104,0) (* Si no hay fichero -> SalvarComo *)
                ELSE
                  BEGIN
                    Assign(Fichero,NomFich);
                    (*$I-*)
                    ReWrite(Fichero);
                    (*$I+*)
                    IF IOResult<>0
                      THEN MessageBox(PVentana,'Error en la creación del archivo.','ERROR',MB_IconStop OR MB_OK)
                      ELSE
                        BEGIN
                          Write(Fichero,Juego);
                          Close(Fichero)
                        END
                  END
            END; (* 103 *)
          104: (* Ficheros_Salvar_Como *)
            BEGIN (* 104 *)
              Proceso := MakeProcInstance(@SalvarComo,Instancia);
              DialogBox(Instancia,'Salvar', PVentana, Proceso);
              FreeProcInstance(Proceso)
            END; (* 104 *)
          105: (* Ficheros_Salir *)
            BEGIN
              IF Juego.Activo
                THEN
                  IF MessageBox(PVentana,'Si termina ahora perderá la partida actual,'+Chr(13)+
                                         '             ¿Salir de todas maneras?','Aviso',MB_IconStop OR MB_YesNo)
                     = IDNO THEN Exit
                            ELSE SendMessage(PVentana,WM_Destroy,0,0)
                ELSE SendMessage(PVentana,WM_Destroy,0,0)
            END; (* 105*)
          106: (* Acción_Opciones_de_Combate *)
            BEGIN (* 106 *)
              Proceso := MakeProcInstance(@OpcionesCombate, Instancia);
              DialogBox(Instancia,'Opciones_Combate', PVentana, Proceso);
              FreeProcInstance(Proceso)
            END; (* 106 *)
          107: (* Acción_Estadisticas *)
            BEGIN (* 107 *)
              Proceso := MakeProcInstance(@Estadisticas, Instancia);
              DialogBox(Instancia,'Estadisticas', PVentana, Proceso);
              FreeProcInstance(Proceso);
              IF HuboCambio THEN InvalidateRect(PVentana,NIL,False)
            END; (* 107 *)
          108: (* Ayuda_Sobre *)
            BEGIN (* 108 *)
              Proceso := MakeProcInstance(@Sobre, Instancia);
              DialogBox(Instancia, 'Sobre', PVentana, Proceso);
              FreeProcInstance(Proceso)
            END; (* 108 *)
          109: (* Ayuda_Ayuda *)
            BEGIN (* 109 *)
              IF ShellExecute(PVentana,nil,'Risko.wri',nil,nil,SW_ShowMaximized)<32 THEN
                MessageBox(PVentana,'Ha ocurrido un error al intentar abrir el fichero de ayuda.',
                  'ERROR',MB_IconInformation OR MB_OK)
            END; (* 109 *)
          301: (* Boton 'Hecho' *)
            BEGIN (* 301 *)
              CASE Juego.Estado OF
                Atacando:
                  IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
                Ganando:
                  BEGIN
                    IF Juego.NumSold>0 THEN
                      BEGIN
                        Juego.Pais[Juego.Batalla[2]].Ejercitos:=Juego.NumSold;
                        PintarPais(Juego.Batalla[1],Negro);
                        PintarPais(Juego.Batalla[2],Negro)
                      END
                  END; (* Ganando *)
                Fortificando:
                  BEGIN
                    (* Retornar los fortificados a ejercitos *)
                    FOR pj:=1 TO 42 DO
                      BEGIN
                        IF Juego.Pais[pj].Fortif=-1 THEN Juego.Pais[pj].Fortif:=0;
                        Inc(Juego.Pais[pj].Ejercitos,Juego.Pais[pj].Fortif);
                        Juego.Pais[pj].Fortif:=0
                      END;
                    (* Terminar con el pais en fortificación *)
                    IF Juego.NumSold>0 THEN
                      BEGIN
                        Juego.Pais[Juego.PFortifica].Ejercitos:=Juego.NumSold;
                        PintarPais(Juego.PFortifica,Negro)
                      END
                  END (* Fortificando *)
              END; (* Case *)
              (* Pasar al siguiente jugador: *)
              IF Juego.Invadio THEN DarCarta(Juego.Poles[Juego.Actual]);
              MoverTurno;
              DibujarTurnos;
              Juego.NumSold:=TotalCobrar(Juego.Poles[Juego.Actual]);
              Juego.Estado:=Repartiendo;
              (* Cobrar cartas si se tienen 5 *)
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>4
                THEN
                  IF Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1
                    THEN
                      BEGIN
                        MessageBox(PVentana,'Tienes más de 4 cartas. Debes cobrar.','Aviso',
                                   MB_IconStop OR MB_OK);
                        Juego.Estado:=Repartiendo;
                        LlamarCartas
                      END
                    ELSE CCOrdenador;
              (* Comprobar si hay soldados para repartir *)
              IF Juego.NumSold>0
                THEN
                  BEGIN
                    PintarSoldados(Juego.NumSold);
                    EnableWindow(Hecho,False);
                    EnableWindow(Fortificar,False)
                  END
                ELSE
                  BEGIN
                    EnableWindow(Fortificar,True);
                    Juego.Estado:=Atacando
                  END;
              (* Activación del menú estadísticas por si se desactivo antes *)
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1
                THEN EnableMenuItem(GetMenu(PVentana),106,MF_Enabled)
                ELSE EnableMenuItem(GetMenu(PVentana),106,MF_Grayed);
              (* Activación del botón cartas si necesario *)
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0
                THEN EnableWindow(Carta,True)
                ELSE EnableWindow(Carta,False)
            END; (* 301 *)
          302: (* Boton 'Fortificar' *)
            {IF ((Juego.Poles[Juego.Actual]<>0)AND(Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1)) THEN}
            BEGIN
              IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
              Juego.PFortifica:=0;
              FOR Juego.i:=1 TO 42 DO Juego.Pais[Juego.i].Recibido:=False;
              EnableWindow(Fortificar,False);
              IF Juego.Jugador[Juego.Poles[Juego.Actual]].NCartas>0
                THEN EnableWindow(Carta,True)
                ELSE EnableWindow(Carta,False);
              Juego.Estado:=Fortificando
            END; (* 302 *)
          303: (* Boton 'Cartas' *)
            IF ((Juego.Poles[Juego.Actual]<>0)AND(Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad=1)) THEN BEGIN (* 303 *)
              LlamarCartas
            END (* 303 *)
          ELSE (* Case PWParam of *)
            VentanaProc := DefWindowProc(PVentana,PMensaje,PWParam,PLParam)
        END; (* Case PWParam y de Wm_Command *)
      WM_NCLButtonDown: (* deshabilitación del movimiento de ventana cuando no minimizada *)
        BEGIN
          IF ((PWParam=HTCaption)AND(NOT IsIconic(PVentana)))
            THEN
            ELSE VentanaProc:=DefWindowProc(PVentana,PMensaje,PWParam,PLParam)
        END;
      WM_Close:
        SendMessage(PVentana,WM_Command,105,0);
      WM_Destroy: (* Cerrar ventana *)
        PostQuitMessage(0);
      WM_Paint: (* Repintado de la ventana *)
        BEGIN (* WM_Paint *)
          BeginPaint(PVentana,Estruct);
          Repintar(Estruct.HDC);
          EndPaint(PVentana,Estruct)
        END (* WM_Paint *)
      ELSE (* Si no se ha procesado el mensaje se realiza el procesamiento por defecto *)
        VentanaProc := DefWindowProc(PVentana,PMensaje,PWParam,PLParam)
    END (* Case PMensaje OF *)
  END; (* VentanaProc *)


BEGIN (* Principal *)
  INSTANCIA := HInstance; (* Para no confundir HInstance con ClaseVentana.hInstance *)
  IF hPrevInst<>0 THEN (* Ya está siendo ejecutada la aplicación Risk *)
    BEGIN
      MessageBox(GetFocus,'El Risko ya se está ejecutando','AVISO',MB_IconAsterisk OR MB_OK);
      HALT(0)
    END;
  (* inicializacion de algunos datos *)
  Ini;
  (* Registramos la clase de ventana llamada 'ClaseRisko' *)
  WITH ClaseVentana DO
    BEGIN
      Style         := CS_ByteAlignWindow OR CS_OwnDC;
      LpfnWndProc   := @VentanaProc;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := HInstance;
      hIcon         := LoadIcon  (INSTANCIA,'RiskIcon2');
      hCursor       := LoadCursor(INSTANCIA,'RiskCursor');
      hbrBackground := GetStockObject(White_Brush);
      lpszMenuName  := 'RiskMenu';
      lpszClassName := 'ClaseRisko'
    END; (* With *)
  IF NOT RegisterClass(ClaseVentana) THEN
    BEGIN
      MessageBox(GetFocus,'Imposible registrar clase','Error',MB_OK);
      HALT(255)
    END;
  (* Creación de la ventana principal del juego. Pertenecerá a la 'ClaseRisko' *)
  Ventana:=CreateWindow('ClaseRisko',VERSION,WS_Maximize OR WS_SysMenu OR WS_MinimizeBox,
                        0,0,640,480,0,0,Instancia,nil);
  (* Creación de los botones que van en la ventana principal *)
  Hecho := CreateWindow(
    'Button','Hecho',Bs_PushButton or ws_Child or ws_visible or ws_Disabled,
    401,8,54,25,Ventana,301,HInstance,nil);
  Fortificar := CreateWindow(
    'Button','Fortificar',Bs_PushButton or ws_Child or ws_visible or ws_Disabled,
    459,8,71,25,Ventana,302,HInstance,nil);
  Carta := CreateWindow(
    'Button','Cartas',Bs_PushButton or ws_Child or ws_visible or ws_Disabled,
    534,8,60,25,Ventana,303,HInstance,nil);
  (* Contexto grafico principal *)
  MDC:=GetDC(Ventana);
  (* Carga de los bitmaps *)
  (* Configuración de la fuente para la ventana *)
  SelectObject(MDC,Fuente);
  SetTextAlign(MDC,TA_Center OR TA_Top);
  (* Mostrar la ventana generando un wm_paint *)
  ShowWindow(Ventana,CmdShow);
  UpDateWindow(Ventana); (* Genera un mensaje WM_Paint *)
  DeleteMenu(GetSystemMenu(Ventana,False),SC_Maximize,MF_ByCommand);
  DeleteMenu(GetSystemMenu(Ventana,False),SC_Size,MF_ByCommand);
  (* Comenzar el risk con el menú de partida nueva *)
  GetMessage(Mensaje, 0, 0, 0);
  TranslateMessage(Mensaje);
  DispatchMessage (Mensaje);
  SendMessage(Ventana,WM_Command,101,0);  (* Partida Nueva *)
  (* Bucle principal del programa, base y nucleo del programa *)
  Juego.Final:=False;
  WHILE NOT Juego.Final DO
    BEGIN
      WHILE NOT PeekMessage(Mensaje,0,0,0,Pm_NoRemove) DO
        IF ((Juego.Poles[2]<>0)AND(Juego.Poles[Juego.Actual]<>0)) THEN
          CASE Juego.Jugador[Juego.Poles[Juego.Actual]].Modalidad OF
            2: BEGIN (* MachoMan *)
                 (* Establecer las opciones de combate *)
                 Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut:=True;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].Lucha:=2;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].NDados:=3;
                 (* Grabar la partida *)
                 {Assign(Fichero,'c:\lp\bp\eu\salva.rsk');
                 Rewrite(Fichero);
                 Write(Fichero,Juego);
                 Close(Fichero);}
                 (* Respuesta según el estado *)
                 CASE Juego.Estado OF
                   Circulando   : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    IF (NOT QuedaSitioEn(Continente)) THEN Continente:=0;
                                    IF ( (TengoQuePonerEn(5))AND(QuedaSitioEn(5)) ) THEN Continente:=5;
                                    IF ( (TengoQuePonerEn(3))AND(QuedaSitioEn(3)) ) THEN Continente:=3;
                                    IF ( (TengoQuePonerEn(2))AND(QuedaSitioEn(2)) ) THEN Continente:=2;
                                    IF ( (TengoQuePonerEn(4))AND(QuedaSitioEn(4)) ) THEN Continente:=4;
                                    IF ( (TengoQuePonerEn(1))AND(QuedaSitioEn(1)) ) THEN Continente:=1;
                                    IF ( (TengoQuePonerEn(6))AND(QuedaSitioEn(6)) ) THEN Continente:=6;
                                    IF Continente=0
                                      THEN
                                        REPEAT
                                          Continente:=Random(6)+1;
                                          Juego.PaisEscogido:=Random(42)+1
                                        UNTIL Juego.Pais[Juego.PaisEscogido].Amo=0
                                      ELSE
                                        REPEAT
                                          Juego.PaisEscogido:=Random(C[Continente].Y-C[Continente].X+1)+
                                                                C[Continente].X
                                        UNTIL Juego.Pais[Juego.PaisEscogido].Amo=0;
                                    ProcesoCirculando(Juego.PaisEscogido);
                                    (* Puesta a cero del continente usado *)
                                    IF Juego.i>42 THEN Continente:=0
                                  END; (* Circulando Y WITH *)
                   Iniciando    : BEGIN
                                    IF Juego.Jugador[Juego.Poles[Juego.Actual]].Continente=0
                                      THEN Juego.Jugador[Juego.Poles[Juego.Actual]].Continente:=HallarContinente;
                                    REPEAT Juego.PaisEscogido:=
                                        Random(c[Juego.Jugador[Juego.Poles[Juego.Actual]].Continente].y
                                              -c[Juego.Jugador[Juego.Poles[Juego.Actual]].Continente].x
                                              +1)+c[Juego.Jugador[Juego.Poles[Juego.Actual]].Continente].x
                                    UNTIL Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual];
                                    ProcesoIniciando(Juego.PaisEscogido,Juego.NumSold)
                                  END; (* Iniciando *)
                   Repartiendo  : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    IF NCartas>2 THEN CCOrdenador;
                                    (* Seleccionamos para repartir y atacar primero algún continente que hayamos perdido *)
                                    FOR pos:=Conts DOWNTO 1 DO
                                      IF ((NOT EsMio(ElCont[pos]))AND(TengoSitioEn(Continente))) THEN Continente:=ElCont[pos];
                                    (* si no se escogio entonces cogemos otro *)
                                    IF ( (Continente=0)OR(NOT TengoSitioEn(Continente)) )
                                      THEN Continente:=HallarContinente;
                                    (* Calcular cuantos caben en el continente *)
                                    m:=CuantosCaben(Continente);
                                    (* Reparto según circustancias *)
                                    IF m>Juego.NumSold THEN m:=Juego.NumSold;
                                    (* reparto de los que caben en el continente *)
                                    FOR pos:=1 TO Juego.NumSold-(Juego.NumSold-m) DO
                                      BEGIN
                                        REPEAT Juego.PaisEscogido:=
                                               Random(c[Continente].y-c[Continente].x+1)+c[Continente].x
                                        UNTIL ((Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual])
                                               AND(Juego.Pais[Juego.PaisEscogido].Ejercitos<99));
                                        ProcesoRepartiendo(Juego.PaisEscogido,1)
                                      END;
                                    (* reparto de los ejércitos que no cabían en el continente *)
                                    FOR pos:=1 TO Juego.NumSold DO
                                      BEGIN
                                        REPEAT Juego.PaisEscogido:=Random(42)+1
                                        UNTIL ((Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual])
                                                AND(Juego.Pais[Juego.PaisEscogido].Ejercitos<99));
                                        ProcesoRepartiendo(Juego.PaisEscogido,1)
                                      END
                                  END; (* Repartiendo *)
                   Atacando     : BEGIN
                                  WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    CalculoDePaises;
                                    (* Ataque a las fronteras por parte de los paises *)
                                    FOR m:=1 TO Random(FinBolsa+1)(* nº de paises que van a atacar *)
                                      DO IF Juego.Poles[2]<>0 THEN (* por si se ganó en la vuelta anterior del for *)
                                      BEGIN
                                        (* el pais que ataca *)
                                        REPEAT
                                          PA:=Random(c[Continente].y-c[Continente].x+1)+c[Continente].x
                                        UNTIL Juego.Pais[Pa].Amo=Juego.Poles[Juego.Actual];
                                        IF Juego.Pais[Pa].Ejercitos>0 THEN
                                          BEGIN
                                            PD:=Random(DatPais[pa].Fronteras)+1;
                                            IF Juego.Pais[DatPais[pa].Frontera[PD]].Amo<>Juego.Poles[Juego.Actual]
                                              THEN (* ¡ AL ATAQUE ! *)
                                                BEGIN
                                                  (* Seleccionamos el nuevo pais que ataca *)
                                                  IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
                                                  Juego.PAtaca:=Pa;
                                                  PintarPais(Juego.PAtaca,Borde);
                                                  (* aquí se responde a la solución del ataque *)
                                                  CASE ProcesoAtacando(DatPais[PA].Frontera[PD]) OF
                                                    1   : LlamarVictoria;
                                                    3,5 : BEGIN
                                                            Juego.Estado:=Repartiendo;
                                                            CCOrdenador
                                                          END
                                                  END (* Case *)
                                                END (* If PD es enemigo *)
                                          END (* If ejercitos de PA>0 *)
                                     END; (* del FOR ppj*)
                                     IF EsMio(Continente) THEN
                                       BEGIN
                                         MeterContinente;
                                         Continente:=0
                                       END;
                                     (* Pasar al siguiente jugador generando un mensaje hecho, si no se ganó ya *)
                                     IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END (* With y Atacando *)
                                  END; (* Atacando *)
                   Ganando      : BEGIN
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END; (* Ganando *)
                   Fortificando : BEGIN
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END (* Fortificando *)
                 END (* Case estado *)
               END; (* MachoMan *)
            3: BEGIN (* Caótico *)
                 (* Establecer las opciones de combate *)
                 Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut:=True;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].Lucha:=2;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].NDados:=1;
                 (* Grabar la partida *)
                 {Assign(Fichero,'c:\lp\bp\eu\salva.rsk');
                 Rewrite(Fichero);
                 Write(Fichero,Juego);
                 Close(Fichero);}
                 (* Respuesta según el estado *)
                 CASE Juego.Estado OF
                   Circulando   : BEGIN
                                    REPEAT Juego.PaisEscogido:=Random(42)+1 UNTIL Juego.Pais[Juego.PaisEscogido].Amo=0;
                                    ProcesoCirculando(Juego.PaisEscogido)
                                  END; (* Circulando *)
                   Iniciando    : BEGIN
                                    REPEAT Juego.PaisEscogido:=Random(42)+1
                                    UNTIL Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual];
                                    ProcesoIniciando(Juego.PaisEscogido,1)
                                  END; (* Iniciando *)
                   Repartiendo  : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    CalculoDePaises;
                                    IF NCartas>2 THEN CCOrdenador;
                                    REPEAT
                                      Juego.PaisEscogido:=BolsaPais[Random(FinBolsa)+1];
                                      {UNTIL Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual];}
                                      ProcesoRepartiendo(Juego.PaisEscogido,1)
                                    UNTIL Juego.NumSold<1
                                  END; (* Repartiendo y WITH *)
                   Atacando     : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    (* Ataque a las fronteras por parte de los paises *)
                                    FOR m:=1 TO Random(FinBolsa+1)(* paises van atacar entre 0 y maximo *)
                                      DO IF Juego.Poles[2]<>0 THEN (* por si se ganó en la vuelta anterior del for *)
                                      BEGIN
                                        (* el pais que ataca *)
                                        PA:=Random(FinBolsa)+1;
                                        IF Juego.Pais[BolsaPais[Pa]].Ejercitos>0 THEN
                                          BEGIN
                                            (* el pais que defiende *)
                                            PD:=Random(DatPais[BolsaPais[pa]].Fronteras)+1;
                                            IF Juego.Pais[DatPais[BolsaPais[pa]].Frontera[PD]].Amo<>Juego.Poles[Juego.Actual]
                                              THEN (* ¡ AL ATAQUE ! *)
                                                BEGIN
                                                  (* pintamos el nuevo pais que ataca *)
                                                  IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
                                                  Juego.PAtaca:=BolsaPais[PA];
                                                  PintarPais(Juego.PAtaca,Borde);
                                                  (* aquí se responde a la solución del ataque *)
                                                  CASE ProcesoAtacando(DatPais[BolsaPais[PA]].Frontera[PD]) OF
                                                    1   : LlamarVictoria;
                                                    3,5 : BEGIN
                                                            Juego.Estado:=Repartiendo;
                                                            CCOrdenador
                                                          END
                                                  END (* Case *)
                                                END (* If PD es enemigo *)
                                          END (* If ejercitos del PA>0 *)
                                     END; (* del FOR ppj*)
                                     (* Pasar al siguiente jugador generando un mensaje hecho, si no se ganó ya *)
                                     IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END; (* With y Atacando *)
                   Ganando      : BEGIN
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END; (* Ganando *)
                   Fortificando : BEGIN
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END (* Fortificando *)
                 END (* Case estado *)
               END; (* Caótico *)
            4: BEGIN (* Neutral *)
                 (* Establecer las opciones de combate *)
                 Juego.Jugador[Juego.Poles[Juego.Actual]].AvanceAut:=True;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].Lucha:=2;
                 Juego.Jugador[Juego.Poles[Juego.Actual]].NDados:=3;
                 (* Grabar la partida *)
                 {Assign(Fichero,'c:\lp\bp\eu\salva.rsk');
                 Rewrite(Fichero);
                 Write(Fichero,Juego);
                 Close(Fichero);}
                 (* Respuesta según el estado *)
                 CASE Juego.Estado OF
                   Circulando   : BEGIN
                                    REPEAT Juego.PaisEscogido:=Random(42)+1
                                    UNTIL Juego.Pais[Juego.PaisEscogido].Amo=0;
                                    ProcesoCirculando(Juego.PaisEscogido)
                                  END; (* Circulando *)
                   Iniciando    : BEGIN
                                    REPEAT Juego.PaisEscogido:=Random(42)+1
                                    UNTIL ( (Juego.Pais[Juego.PaisEscogido].Amo=Juego.Poles[Juego.Actual])
                                            AND(TengoEnemigosEnLasFronteras(Juego.PaisEscogido)) );
                                    ProcesoIniciando(Juego.PaisEscogido,1)
                                  END; (* Iniciando *)
                   Repartiendo  : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    CalculoDePaises;
                                    IF NCartas>2 THEN CCOrdenador;
                                    (* Eliminar de lista paises perdidos que me pertenecen *)
                                    WHILE ((Perdidos>0)AND(Juego.Pais[ElPerdido[Perdidos]].Amo=Juego.Poles[Juego.Actual]))
                                      DO Dec(Perdidos);
                                    (* Comprobar si hay paises que recuperar *)
                                    IF Perdidos=0
                                      THEN (* No hay nada que recuperar -> reparto aleatorio *)
                                        REPEAT
                                          REPEAT
                                            Juego.PaisEscogido:=BolsaPais[Random(FinBolsa)+1]
                                          UNTIL TengoEnemigosEnLasFronteras(Juego.PaisEscogido);
                                          ProcesoRepartiendo(Juego.PaisEscogido,1)
                                        UNTIL Juego.NumSold<1
                                      ELSE (* Tiene paises en espera de reconquista *)
                                        BEGIN
                                          HallarMejorCamino(ElPerdido[Perdidos],Juego.Camino);
                                          (* ponemos todos los ejércitos que podemos al comienzo del camino *)
                                          m:=0;
                                          WHILE NOT(ProcesoRepartiendo(Juego.Camino[0],Juego.NumSold-m)) DO Inc(m);
                                          (* Si sobran se ponen aleatoriamente por ahí *)
                                          IF m<>0 THEN
                                            BEGIN
                                              Juego.NumSold:=m;
                                              REPEAT
                                                Juego.PaisEscogido:=BolsaPais[Random(FinBolsa)+1];
                                                ProcesoRepartiendo(Juego.PaisEscogido,1)
                                              UNTIL Juego.NumSold<1
                                            END (* IF m<>0 *)
                                        END (* else *)
                                  END; (* Repartiendo y WITH *)
                   Atacando     : WITH Juego.Jugador[Juego.Poles[Juego.Actual]] DO BEGIN
                                    (* Inicializar paises a fortificar *)
                                    FOR Juego.PAFS:=1 TO 10 DO Juego.PAF[Juego.PAFS]:=0;
                                    Juego.PAFS:=0;
                                    (* Inicializar m *)
                                    m:=Perdidos;
                                    WHILE m>0 DO
                                      BEGIN
                                        (* Primero vamos hasta el pais de partida->el último nos robaron *)
                                        (* Hallamos el camino mejor hasta dicho pais *)
                                        HallarMejorCamino(ElPerdido[m],Juego.Camino);
                                        (* Vamos a recorrerlo mientras podamos *)
                                        Juego.CaminoPos:=0;
                                        REPEAT
                                          (* Escoger el pais que Ataca *)
                                          IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
                                          Juego.PAtaca:=Juego.Camino[Juego.CaminoPos];
                                          IF Juego.Pais[Juego.PAtaca].Ejercitos>0 THEN PintarPais(Juego.PAtaca,Borde);
                                          (* Seguir el camino *)
                                          Inc(Juego.CaminoPos);
                                          IF Juego.Pais[Juego.PAtaca].Ejercitos>0
                                            THEN
                                              CASE ProcesoAtacando(Juego.Camino[Juego.CaminoPos]) OF
                                                1   : LlamarVictoria;
                                                3,5 : BEGIN
                                                        Juego.Estado:=Repartiendo;
                                                        CCOrdenador
                                                      END
                                              END (* Case *)
                                            ELSE Juego.PAtaca:=0
                                        UNTIL (   (Juego.PAtaca=0) (* Equivale a que no queden ejércitos *)
                                               OR (Juego.Pais[Elperdido[m]].Amo=Juego.Poles[Juego.Actual])
                                               OR (Juego.Poles[2]=0) );
                                        IF (Juego.Pais[Elperdido[m]].Amo=Juego.Poles[Juego.Actual])
                                          THEN BEGIN
                                                 Inc(Juego.PAFS);
                                                 Juego.PAF[Juego.PAFS]:=ElPerdido[m];
                                                 Dec(m);
                                                 (* escogemos manualmente el primer pais que ataca *)
                                                 IF Juego.PAtaca<>0 THEN PintarPais(Juego.PAtaca,Negro);
                                                 Juego.PAtaca:=ElPerdido[m+1]
                                               END
                                          ELSE (* Condición de salida *)  m:=0;

                                        (* Ahora seguimos la ruta de perdidos hasta donde podamos *)
                                        WHILE ( (m>0)
                                                AND(Juego.Poles[2]<>0)
                                                AND(Juego.PAtaca<>0) (* equivale queden ejércitos *)
                                                AND(SonVecinos(ElPerdido[m+1],ElPerdido[m])) ) DO
                                          BEGIN
                                            (* Seguir perdidos *)
                                            IF Juego.Pais[Juego.PAtaca].Ejercitos>0
                                              THEN
                                                BEGIN
                                                  (* marcar el pais que Ataca *)
                                                  PintarPais(Juego.PAtaca,Borde);
                                                  CASE ProcesoAtacando(ElPerdido[m]) OF
                                                    1   : LlamarVictoria;
                                                    3,5 : BEGIN
                                                            Juego.Estado:=Repartiendo;
                                                            CCOrdenador
                                                          END
                                                  END (* Case *)
                                                END (* Then *)
                                              ELSE Juego.PAtaca:=0;
                                            IF Juego.Pais[Elperdido[m]].Amo=Juego.Poles[Juego.Actual]
                                              THEN BEGIN
                                                     Inc(Juego.PAFS);
                                                     Juego.PAF[Juego.PAFS]:=ElPerdido[m];
                                                     Dec(m)
                                                   END;
                                            (* Escoger el pais que Ataca *)
                                            IF Juego.PAtaca<>0 THEN
                                              BEGIN
                                                PintarPais(Juego.PAtaca,Negro);
                                                Juego.PAtaca:=ElPerdido[m+1]
                                              END
                                          END; (* while halla perdidos,
                                                  queden ejércitos suficientes,
                                                  no hallamos ganado
                                                  y el siguiente sea frontera *)
                                          (* Condición de reserva para el fin del bucle principal *)
                                          IF Juego.PAtaca=0 THEN m:=0
                                      END; (* While m>0 *)
                                    (* Decrementamos perdidos hasta donde sea necesario *)
                                    WHILE ( (Perdidos>0)AND(Juego.Pais[ElPerdido[Perdidos]].Amo=Juego.Poles[Juego.Actual]) )
                                      DO Dec(Perdidos);
                                    (* Mandar un Fortificar *)
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,302,2);
                                  END; (* Atacando y WITH *)
                   Ganando      : BEGIN
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2)
                                  END; (* Ganando *)
                   Fortificando : BEGIN
                                    Juego.NumSold:=0;
                                    FOR m:=1 TO Juego.PAFS DO IF Juego.Pais[Juego.PAF[m]].Ejercitos>0 THEN
                                      BEGIN
                                        Suma:=Juego.Pais[Juego.PAF[m]].Ejercitos;NFAF:=1;
                                        FOR pos:=1 TO DatPais[Juego.PAF[m]].Fronteras DO
                                          IF Juego.Pais[DatPais[Juego.PAF[m]].Frontera[pos]].Amo=Juego.Poles[Juego.Actual]
                                            THEN BEGIN
                                                   Inc(Suma,Juego.Pais[DatPais[Juego.PAF[m]].Frontera[pos]].Ejercitos);
                                                   Inc(NFAF)
                                                 END;
                                        FOR pos:=1 TO DatPais[Juego.PAF[m]].Fronteras DO
                                          IF Juego.Pais[DatPais[Juego.PAF[m]].Frontera[pos]].Amo=Juego.Poles[Juego.Actual]
                                            THEN Juego.Pais[DatPais[Juego.PAF[m]].Frontera[pos]].Ejercitos:=Suma DIV NFAF;
                                        Juego.Pais[Juego.PAF[m]].Ejercitos:=(Suma DIV NFAF)+(Suma MOD NFAF);
                                        FOR pos:=1 TO DatPais[Juego.PAF[m]].Fronteras DO
                                          IF Juego.Pais[DatPais[Juego.PAF[m]].Frontera[pos]].Amo=Juego.Poles[Juego.Actual]
                                            THEN PintarPais(DatPais[Juego.PAF[m]].Frontera[pos],Negro);
                                        PintarPais(Juego.PAF[m],Negro)
                                      END; (* FOR de PAF[m] *)
                                    IF Juego.Poles[2]<>0 THEN SendMessage(Ventana,WM_Command,301,2);
                                  END (* Fortificando *)
                 END (* Case estado *)
               END  (* Neutral *)
          END; (* Case Modalidad *)
      (* Al llegar aquí es que hay un mensaje pendiente *)
      IF GetMessage(Mensaje,0,0,0)
        THEN
          BEGIN
            TranslateMessage(Mensaje);
            DispatchMessage(Mensaje)
          END
        ELSE Juego.Final:=True (* Se ha pedido salir *)
  END; (* WHILE NOT Juego.Final *)
  (* Finalización del programa: borrar variables y dar un código de salida *)
  DeleteObject(Dibujo);
  DeleteObject(Creditos);
  DeleteObject(Uno);
  DeleteObject(Cinco);
  DeleteObject(Diez);
  DeleteObject(Veinte);
  DeleteObject(L);
  DeleteObject(Tu);
  DeleteObject(Caos);
  DeleteObject(Macho);
  DeleteObject(Neutral);
  DeleteObject(Soldado);
  DeleteObject(Caballo);
  DeleteObject(Canon);
  DeleteObject(Fuente);
  DeleteObject(FuenteGrande);
  DeleteObject(Borde);
  DeleteObject(Negro);
  DeleteObject(Azul);
  DeleteObject(Dorso);
  DestroyCursor(No);
  DestroyCursor(Ok);
  FOR Juego.i:=1 TO MaxDibu DO DeleteObject(Brocha[Juego.i].RGB); (* borrar los catorce pinceles *)
  FOR Juego.i:=1 TO 42 DO DeleteObject(DatPais[Juego.i].Grafo);
  FOR Juego.i:=1 TO 42 DO FOR Juego.j:=1 TO 7 DO IF Lugares[Juego.i,Juego.j]<>0 THEN DeleteObject(Lugares[Juego.i,Juego.j]);
  Halt(Mensaje.WParam)
END. (* Principal *)
