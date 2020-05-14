(*************************************************************************)
(*  UNIDADE BMP                                                          *)
(*    Permite cargar e salvar arquivos *.bmp                             *)
(*************************************************************************)
UNIT BMP;
{$S-,R-}

INTERFACE
  USES WinTypes, WinProcs;

  FUNCTION CargaFicheiroBMP(PFicheiro: PChar):HBitmap;
  (* Lee un arquivo bmp (ruta e nome completo) e devolve o handle ou un
     0 se se produce un erro *)

 { FUNCTION SalvaFicheiroBMP(PFicheiro:PChar; HBM:HBitmap):Integer;
  (* Salva no arquivo PFicheiro (ruta e nome completo) o bitmap HBM *)
 }

IMPLEMENTATION
  PROCEDURE AHIncr; far; external 'KERNEL' index 114;
  CONST
    UnaIO  = 32768;  (* nº de bytes por operación IO realizada *)
    TipoBM = $4D42;  (* ='BM'= Tipo Bitmap *)
  TYPE
    PtrRec = RECORD
               Lo,Hi:Word
             END;
    FuncIO = FUNCTION(PFich:integer; Buf:PChar; PTam:Integer): Word;

(****************************)
(* funcións e procedementos *)
(****************************)
(**************************************************************************)
  FUNCTION IOMultiple(PFuncIO:FuncIO; PFich:Integer; PPtr:Pointer;
                      PTam:Longint):Word;
  (* Realiza operacións IO en trozos de tamaño UnaIO para evitar cruces
     de sexmento *)
    VAR
      L,N:Longint;  (* L: nº total de bytes; N: bytes da pasada actual *)
    BEGIN
      IOMultiple:=1;
      L:=0;
      WHILE L<PTam DO
        BEGIN
          N:=PTam-L;
          IF N>UnaIO THEN N:=UnaIO;
          IF PFuncIO(PFich,
          (* calculamos o sexmento e desprazamento alcanzados.
          O Hi word de P conten o sexmento inicial.
          Pensa no seguinte como facendo o módulo aritmético do tamaño do
            sexmento, xa que o tamaño dun sexmento enche un word
          O Hi word de L conten o numero de sexmentos cruzados
            (o tamaño dun sexmento enche o Lo word, polo tanto,  Hi word
            rolará segundo os sexmentos vaian sendo enchidos).
            Multipicar por Ofs(AHIncr) para obter o numero usado para
            indicar este numero de sexmento.
          O Lo word de L conten o numero de bytes xa pasados no presente
            sexmento. *)
	           Ptr(PtrRec(PPtr).Hi + PtrRec(L).Hi * Ofs(AHIncr),
                   PtrRec(L).Lo),
                   Integer(N)) (* facemos un type-casting de N *)
          <> N THEN
            BEGIN
             IOMultiple:=0;
             EXIT; (* remate anormal ¿? *)
            END;
          Inc(L,N)
        END (* While *)
    END; (* function IOMultiple *)
(**************************************************************************)

(**************************************************************************)
  function _LFileSize(F : integer) : longint;
  {- an equivalent to TP's FileSize() function }
  var
    CurPos : longint;
  begin
    CurPos := _llseek(F,0,1);
    _LFileSize := _llseek(F,0,2);
    _llseek(F,CurPos,0);
  end;
(**************************************************************************)

(**************************************************************************)
  (* Lee un arquivo bmp (ruta e nome completo) e devolve o handle ou un
     0 se se produce un erro *)
  FUNCTION CargaFicheiroBMP(PFicheiro: PChar):HBitmap;
    VAR
      Fich:Integer;         (* Handle dun ficheiro *)
      H:THandle;            (* Handle de memoria para o bitmap *)
      DC:HDC;               (* DC para a aplicacion *)
      Tam,N:Longint;        (* Tamaño do bitmap, tamaño da espefic. da cor *)
      P: PBitmapInfo;	    (* Información dun fichero BMP *)
      Cab:TBitmapFileHeader;(* Cabeceira do ficheiro BMP *)
    BEGIN (* CargaFicheiroBMP *)
      CargaFicheiroBMP:=0;
      Fich:=_LOpen(PFicheiro, of_Read);
      IF Fich=-1 THEN EXIT;

      (* ler a cabeceira do bitmap *)
      IF (_LRead(Fich,@Cab,SizeOf(Cab))<>SizeOf(Cab)) OR
        (Cab.bfType<>TipoBM) THEN
        BEGIN
          _LClose(Fich);
          EXIT;
        END;

      (* ler o resto do ficheiro *)
      Tam:=_LFileSize(Fich)-SizeOf(TBitmapFileHeader);
      H:=GlobalAlloc(gmem_Moveable,Tam); (* reservar a memoria *)
      IF H=0 THEN
        BEGIN
          _LClose(Fich);
          EXIT;
        END;

      P:=GlobalLock(H);	(* Bloqueamos H e obtemos o punteiro a H *)

      IF (IOMultiple(_LRead,Fich,P,Tam)<>0) AND
        (P^.bmiHeader.biSize=SizeOf(TBitmapInfoHeader)) THEN
      BEGIN
        (* Calcular o desprazamento dende o comezo de P^
           donde comeza a imaxe actual *)
        N:=Cab.bfOffBits-SizeOf(TBitmapFileHeader);

        (* creamos o bitmap propiamente dito *)
        DC:=GetDC(0);
        CargaFicheiroBMP:=CreateDIBitmap(DC,P^.bmiHeader,
          cbm_Init, Ptr(PtrRec(P).Hi,N),P^, dib_RGB_Colors);

        (* limpiamos *)
        ReleaseDC(0,DC);
      END;

      GlobalUnlock(H);
      GlobalFree(H);
      _LClose(Fich);
    END; (* CargaFicheiroBMP *)
(**************************************************************************)


  { Write a bitmap to file (full pathname).
    Returns 0 if error else non-zero.
  }
(*  function StoreBitmapFile(FileName: PChar; HBM: HBitmap): Integer;
    var
      BM:   TBitmap;		{ Bitmap information }
      BFH:  TBitmapFileHeader;	{ Bitmap file information }
      BIP:  PBitmapInfo;		{ Part of bitmap file information }
      DC:   HDC;			{ Drawing context }

      HMem: THandle;		{ Handle to memory for bitmap }
      Buf:  Pointer;		{ Memory for bitmap }

      ColorSize, DataSize: Longint; { Size needed to store Color/Data }
      BitCount: Word;		{ Number of bits per pixel }
      FP: Integer;		{ File }

    { Takes the size in bits and returns the (aligned) size in bytes.
      Bitmap data format requires word alignment.
    }
    function bmAlignDouble(Size: Longint): Longint;
    begin
      bmAlignDouble := (Size + 31) div 32 * 4;
    end;

  begin
     StoreBitmapFile := 0;
     { Get the information about the Bitmap }
     if GetObject(HBM, SizeOf(TBitmap), @BM) = 0 then Exit;

     BitCount := bm.bmPlanes * bm.bmBitsPixel;
     if (BitCount <> 24) then
       ColorSize := SizeOf(TRGBQuad) * (1 shl BitCount)
     else
       ColorSize := 0;
     DataSize := bmAlignDouble(bm.bmWidth * BitCount) * bm.bmHeight;

     { Create the file }
     FP := _lcreat(FileName, 0);
     if FP = -1 then Exit;

     { Allocate memory for the bitmap info structure }
     GetMem(BIP, SizeOf(TBitmapInfoHeader) + ColorSize);
     if BIP <> nil then
     begin
       { Fill in the Bitmap info header }
       with BIP^.bmiHeader do
       begin
         biSize := SizeOf(TBitmapInfoHeader);
         biWidth := bm.bmWidth;
         biHeight := bm.bmHeight;
         biPlanes := 1;
         biBitCount := BitCount;
         biCompression := 0;
         biSizeImage := DataSize;
         biXPelsPerMeter := 0;
         biYPelsPerMeter := 0;
         biClrUsed := 0;
         biClrImportant := 0;
       end;

       { Fill in the file header }
       with BFH do
       begin
         bfOffBits := SizeOf(BFH) + SizeOf(TBitmapInfo) + ColorSize;
         bfReserved1 := 0;
         bfReserved2 := 0;
         bfSize :=  bfOffBits + DataSize;
         bfType := BMType;
       end;

       { Create the memory Bitmap }
       HMem := GlobalAlloc(gmem_Fixed, DataSize);
       if HMem <> 0 then
       begin
         Buf := GlobalLock(HMem);

         { Get the bitmap bits in device independent format }
         DC := GetDC(0);
         if GetDIBits(DC, hbm, 0, DataSize, Buf, BIP^, dib_RGB_Colors) <> 0 then
         begin
           ReleaseDC(0, DC);
           { Write to file }
           _lwrite(FP, @BFH, SizeOf(BFH));
           _lwrite(FP, PChar(BIP), SizeOf(TBitmapInfo) + ColorSize);
           HugeIO(_lwrite, FP, Buf, DataSize);
           StoreBitmapFile := 1;
         end;

         { Clean up }
         GlobalUnlock(HMem);
         GlobalFree(HMem);
       end;

       FreeMem(BIP, SizeOf(TBitmapInfoHeader) + ColorSize);
     end;

     _lclose(FP);

  end;
  *)

  end.
