CREATE OR REPLACE PACKAGE SGCVNZ.PQOUTRETORNOMC_NGTA IS
/***************************************************************************************************
**********************   Proyecto : Procesadora Venezuela   ****************************************
**********************   Proposito: Outgoing Mastercard Rechazos y Retornos ************************
 Historial
 =========
 Persona                  Fecha           Comentarios
 --------------------     -----------     ------------
 Walter Cisneros E.     04/Dec/2007     inicio Creacion del package
 Eduardo Molina         04/05/2017      Incluir campo DE14 fecha de exp tar
 Carlos Brito           05/09/2017      Cambio DE 22 Subcampo 4 Tipo de terminal
 Francisco Vasquez      14/01/2020      Salientes Retorno para Naiguata IPR 1302
****************************************************************************************************/

--MODIFICACIONES CAMBIOS MANDATORY IPR 1132-----------


 TYPE Mapa_Bits is Table Of Char(1);
 TYPE ArrayLuhn is Table Of number(1);


 TYPE s is record(
      s1  char(1)  :='',
      s2  char(1)  :='',
      s3  char(1)  :='',
      s4  char(1)  :='',
      s5  char(1)  :='',
      s6  char(1)  :='',
      s7  char(1)  :='',
      s8  char(1)  :='',
      s9  char(1)  :='',
      s10 char(1)  :='',
      s11 char(1)  :='',
      s12 char(1)  :=''
      );

 type vb_CardAceptorBusiness is record(vbCab_61 boolean := false,
                                       vbCab_62 boolean := false,
                                       vbCab_63 boolean := false,
                                       vbCab_67 boolean := false,
                                       vbCab_73 boolean := false,
                                       vbCab_75 boolean := false);



 TYPE Msg_1240C IS RECORD  -- Compras
     (idemen_p00_actc    varchar2 (4)    :='',     -- indicador de tipo de mensaje
      bitpri_actc        varchar2(16)    :='',     -- Primer Mapa De Bits
      bitsec_actc        varchar2(16)    :='',     -- Segundo Mapa De Bits
      lontar_l02_actc    varchar2(19)    :='',     -- PAN o Numero de Tarjeta Longitud
      numtar_p02_actc    varchar2(19)    :='',     -- PAN o Numero de Tarj
      bin_tarjeta        varchar2(6)     :='',
      codpro_p03_actc    varchar2(6)     :='',     -- Codigo de procesamiento
      imptra_p04_actc    varchar2(12)    :='',     -- Monto de transaccion
      timloc_p12_actc    varchar2(12)    :='',     -- fecha y hora de la transaccion
      feccad_p14_actc    varchar2(4)     :='',     -- Fecha de Expiracion
      punser_p22_actc    varchar2(12)    :='',     -- Codigo del Punto de servicio
      numsec_p23_actc    varchar2(3)     :='',
      de_24              varchar2(3)     :='',     -- Codigo de funcion
      de_25              varchar2(4)     :='',     -- Codigo de Razon
      de_26              varchar2(4)     :='',     --pls_integer     :=0,      -- Codigo de Negocio del establecimiento
      de_30              varchar2(24)    :='',     -- Monto original
        de_30s1          varchar2(12)    :='',     -- Monto original de la transaccion
        de_30s2          varchar2(12)    :='',     -- Monto original de la conciliacion
      de_31              varchar2(25)    :='',     -- Datos referentes al adquiriente
      de_32              varchar2(13)    :='',
      de_33              varchar2(13)    :='',
      de_37              varchar2(12)    :='',     -- Numero de referencia de la peticion
      de_38              varchar2(6)     :='',     -- Codigo de autorizacion
      de_40              varchar2(3)     :='',
      de_41              varchar2(8)     :='',     -- Identificacion del terminal
      de_42              varchar2(15)    :='',     -- codigo de comercio
      de_43               varchar2(267)   :='',
      nomest_p43         varchar2(83)    :='',     -- nombre del comercio
      dircomercio_p43    varchar2(83)    :='',     -- direccion del comercio
      locest_p43         varchar2(83)    :='',     -- Localidad del comercio
      longdat_p43        char(2)         :='',
      cod_postal_p43     varchar(10)     :='',     -- codigo postal
      cod_estado_p43     varchar(3)      :='',     -- codigo de estado
      paiest_p43         varchar2(3)     :='',     -- pais del comercio
      londat_P48         varchar(3)      :='',     -- Longitud del p48
      De48               varchar2(107)   :='',     -- longitud de dato, dato adicional
         pds0002_de48    varchar2(3)     :='',
         pds0003_de48    varchar2(3)     :='',
         pds0025_de48    varchar2(14)    :='',
         pds0148_de48    varchar2(21)    :='',
         pds0149_de48    varchar2(13)    :='',
         pds0158_de48    varchar2(19)    :='',
         pds0165_de48    varchar2(38)    :='',
         pds0262_Ide48    varchar2(1)     :='',
         pds0262_de48    varchar2(8)     :='',
      de_49              varchar2(3)     :='',     -- Codigo de la moneda de la transaccion
      moncon_p50_actc    varchar2(3)     :='',     -- codigo moneda de conciliacion
      londat_p55         varchar2(3)     :='',     -- Longitud del p55
      De55               varchar2(256)   :='',     -- Composici¿n del De55
         t9f26_de55      varchar2(80)     :='',
         t9f27_de55      varchar2(80)     :='',
         t9f10_de55      varchar2(80)    :='',
         t9f37_de55      varchar2(80)     :='',
         t9f36_de55      varchar2(80)     :='',
         t95_de55        varchar2(80)     :='',
         t9a_de55        varchar2(80)     :='',
         t9c_de55        varchar2(80)     :='',
         t9f02_de55      varchar2(80)     :='',
         t5f2a_de55      varchar2(80)     :='',
         t82_de55        varchar2(80)     :='',
         t9f1a_de55      varchar2(80)     :='',
         t9f03_de55      varchar2(80)     :='',
      de_63              varchar2(19)    :='',     -- transaction life cicle
      de_71              varchar2(8)     :='',     -- numero de mensaje en el lote
      londat_de72        varchar(3)      :='',
      de_72              varchar(50)     :='',
      de_94              varchar2(13)    :='',     -- valor del campo: presen_p33_actc
      de_95              varchar2(12)    :='',     -- Datos referios al emisor de la tarjeta
      cod_ird            varchar2(2)     :='',     -- codigo de IRD
      cod_mcc            varchar2(4)     :='',     -- codigo MCC
      cod_tcc            char(1)         :='',     -- codigo TCC
      isnum_nacional     pls_integer     :=0,      -- Es tarjeta Nacional?
      isBetweenFiveday   boolean         := false, -- la transaccion realizada, es enviada dentro de los 5 dias calendario?
      isBetweenThirtyday boolean         := false, -- la transaccion realizada, es enviada dentro de los 30 dias calendario
      cod_producto_gcms  varchar2(3)     :='',
      cod_licen_producto varchar2(3)     :='',
      cod_pais           varchar2(3)     :='',
      cod_region         varchar2(1)     :='',
      cod_programa       varchar2(3)     :='',
      ImpTran            varchar2(12)    :='',
      MonTran            varchar2(3)     :='',
      TransationID       varchar2(10)    :='',
      SourceAmount       varchar2(12)    :='',
      currenCode         varchar2(3)     :='',
      FecPresentacion    varchar2(6)     :='' );







TYPE r_Mtotot_1240 IS RECORD  --Record para calculo de Sumas de 1240's
     (MontoTotal     pls_integer:=0,
      TotalMensaje   pls_integer:=0
   );




 TYPE Msg_1644 IS RECORD -- mensajes administrativos
     (idemen_p00_actc    varchar2(4)    :='',   --indicador de tipo de mensaje
      bitpri_actc        varchar2(16)   :='',   -- Primer Mapa De Bits
      bitsec_actc        varchar2(16)   :='',   -- Segundo Mapa De Bits
      codfun_p24_actc    varchar2(3)    :='',   -- Codigo de funcion
      codraz_p25_actc    varchar2(4)    :='',   -- Codigo de Razon
      codact_p26_actc    varchar2(4)    :='',   -- Codigo de Negocio del establecimiento
      impori_p30_actc    varchar2(24)   :='',   -- Monto original de transacion
      refadq_p31_actc    varchar2(23)   :='',   -- Datos referentes al adquiriente
      londat_p48_actc    varchar2(3)    :='',   -- longitud de dato, dato adicional
      de_p48             varchar2(71)   :='',
        pds0105_p48      varchar2(32)   :='',
        pds0122_p48      varchar2(8)    :='',
        pds0301_p48      varchar2(23)   :='',
        pds0306_p48      varchar2(15)   :='',
      nummen_p71_actc    varchar2(8)    :='',   -- numero de mensaje en el lote
      delement94_actc    varchar2(11)   :='',   -- valor del campo: presen_p33_actc
      numemi_p95_actc    varchar2(10)   :=''    -- Datos referios al emisor de la tarjeta
  );

--MODIFICACIONES se anexa campo DE14 ipr 1137-----------
TYPE Msg_1740 IS RECORD  -- Cobros pagos
     (mti                varchar2 (4)    :='',     -- indicador de tipo de mensaje
      bitpri             varchar2(16)    :='',     -- Primer Mapa De Bits
      bitsec             varchar2(16)    :='',     -- Segundo Mapa De Bits
      de02               varchar2(23)    :='',     -- PAN o Numero de Tarj
      de03               varchar2(6)     :='',     -- Codigo de procesamiento
      de04               varchar2(12)    :='',
      de14               varchar2(4)     :='',     -- Fecha de Expiracion  MODIGICADO GLOBALR 16-03-2017 ipr 1137 Inclusi¿n del DE14
      de24               varchar2(3)     :='',     -- Codigo de funcion
      de25               varchar2(4)     :='',     -- Codigo de Razon
      de30               varchar2(24)    :='',     -- Monto original
        de30s1           varchar2(12)    :='',     -- Monto original de la transaccion
      de31               varchar2(23)    :='',     -- Datos referentes al adquiriente
        de31_pre         varchar2(22)    :='',
      de33               varchar2(11)    :='',
      de48               varchar2(124)   :='',     -- longitud de dato, dato adicional
        de48_londat      varchar(3)      :='',     -- Longitud del p48
        de48_pds0025     varchar2(14)    :='',
        pds0137_lon      varchar(3)      :='',
        de48_pds0137     varchar2(27)    :='',
        pds0137_cad      varchar2(17)    :='',
        digCheck         pls_integer     :=0,
        de48_pds0148     varchar2(21)    :='',
        de48_pds0149     varchar2(6)     :='',
        de48_pds0165     varchar2(38)    :='',
        de48_pds0230     varchar2(8)     :='',
        de48_pds0264     varchar2(4)     :='',
      de49               varchar2(3)     :='',
      de71               varchar2(8)     :='',     -- numero de mensaje en el lote
      de73               varchar2(6)     :='',     -- longitud de dato DE94
      de93               varchar2(24)    :='',
      de94               varchar2(11)    :='',     -- valor del campo: presen_p33_actc
      de95               varchar2(10)    :='',
      isnum_nacional     pls_integer     :=0,
      cod_ird            varchar2(2)     :='',     -- codigo de IRD
      cod_mcc            varchar2(4)     :='',     -- codigo MCC
      cod_tcc            char(1)         :='',     -- codigo TCC
      isBetweenFiveday   boolean         :=false,  -- la transaccion realizada, es enviada dentro de los 5 dias calendario
      cod_producto_gcms  varchar2(3)     :='',
      cod_licen_producto varchar2(3)     :='',
      ImpTran            varchar2(12)    :='',
      MonTran            varchar2(3)     :='');


function f_main(psfecha varchar2, pcodhcierre CHAR,pRepro char:='N')return varchar2;

procedure p_mainGen_byBank(pFecha char,pFecha6 char,pBankChar char,
                           pIdProc number,pDirOut char,pRepro char:='N',
                           pcodhcierre CHAR,pfechad date,pfile out varchar);



procedure p_GenRegCompra(pfecha  varchar2, pbanco varchar2,pcodhcierre CHAR);

procedure p_GenRegDevolucion(pfecha  varchar2, pbanco varchar2,pcodhcierre CHAR);

procedure p_GenRegAnulacion(pfecha  varchar2, pbanco varchar2,pcodhcierre CHAR);

procedure p_GenReg1644(vcodfuncion number,vfecha varchar2, vbanco varchar2);

procedure p_GenReg1740(pfecha  varchar2, pbanco varchar2);

procedure p_FindPostalCode(pcodComercio char, pcodePostal out varchar2, pCodEstado out number);

function f_IniMapaBits return mapa_bits;

function f_IniArrayLuhn return ArrayLuhn;

function f_Bin_Hex(bin varchar2) return char;

function f_Gen_MapaHex2(matrizBin in out Mapa_Bits) return varchar;

function f_ini_msg1240C return Msg_1240C;

function f_ini_msg1740 return Msg_1740;

function f_iniCardAceptorBusiness return vb_CardAceptorBusiness;

function f_findTipoCambio(pbanco varchar2,psfecha varchar2)return pls_integer;

function f_FindTarjetaNacional(bin_tarjeta varchar2,pBanco char )return pls_integer;

function f_FindCodigoRazon(pCodRazon number, pNumTarjeta varchar2 )return varchar2;

function f_AtLeastOne(pfecha varchar2,pbanco varchar2)return pls_integer;

procedure p_findTransOriginal(pbanco varchar2, p31 varchar2,pNumTarjeta varchar2,
                               pImpTran out number, pMonTran out varchar2);

procedure p_findTransactionIdDisputas(p31 varchar2, pNumTarjeta varchar2, pTransationID out varchar2,
                                       pSourceAmount out varchar2,Pcurrencode out varchar2,Pinc_doc_ind out varchar2);

procedure p_getDataInternacional(bin_tarjeta varchar2,pBanco char, pgcms out varchar2,plic_prod out varchar2,pcod_pais out varchar2,pcod_region out varchar2);

procedure p_findird(pin_mcc varchar2, pout_mcc out varchar2, pird out pls_integer, ptcc out varchar2);

function f_IsDate(pstring varchar2)return boolean;

function f_isBetweenFiveDay(pfecha date) return boolean;

function f_isBetweenThirtyDay(pfecha date) return boolean;

function f_GenCheckDigit(pCadena varchar2) return pls_integer ;

function f_getIdBankDestino(bin_tarjeta varchar2,pBanco char)return varchar2;

procedure p_Insert_CtlProcMc(pIDProc number, pIdAdq varchar2,pEstado char,pNumSec number, pfecha date);

--function f_getSecuencia(pbanco char, pNomProc char,pRepro char:='N',pfecha date) return pls_integer;
function f_getSecuencia(pfecha date) return pls_integer;

end;
/

GRANT EXECUTE ON SGCVNZ.PQOUTRETORNOMC_NGTA TO ROLE_SOPTECN;
CREATE OR REPLACE PACKAGE BODY SGCVNZ.PQOUTRETORNOMC_NGTA IS
    vIDFile         utl_file.file_type;
    vFile           varchar2(50);
    vmtotot_1240    r_mtotot_1240;
    vPaso           varchar2(10);
    vIdProc         pls_integer;
    vErrMsg         varchar2(200);
    vRetC           varchar2(100);
    vErrCod         varchar2(2);
    vOraCode        pls_integer;
    vOraErr         varchar2(200);
    eFinError       exception;
    eFinWarning     exception;
    vcad1240        clob;
    vcad1644        clob;
    vNumSecReg      pls_integer := 0;
    vb_isDate       boolean     := true;
--    vTipocambio     number(8,3) := 0;
    vMoney          char(3)     := '';
    vSecControl     pls_integer := 0;
    vBank4          varchar2(4) := '';
    vMontoTotal     number(16)  := 0;
    vtotalMensaje   pls_integer := 0;
    vCantRegMer     pls_integer := 0;
    vCantRegPro     pls_integer := 0;
    vEntorno        char(4)     := '';
    vTipFileMC      char(1)     := '';





FUNCTION f_main(psfecha varchar2,
                pcodhcierre CHAR,
                pRepro char:='N') RETURN varchar2 IS

    vDirOUT          varchar2(100)   :='DIR-OUT'; ---borrar
    vfechac6         varchar2(6)     :='';
    dfecha           date;
    vArchivo         varchar2(50)    :='';
    vRetorno         varchar2(100)   :='';

BEGIN
    vFechac6    := substr(psFecha,3,6);
    dfecha      := TO_DATE(psfecha,'YYYYMMDD');
    --get money code
    vMoney      := pqcomercios.gcw_f_getmonedavig(sysdate);

    --get IDPROC
    vPaso      := 'Paso 01';
    vIDProc    := pqmonproc.InsMonProc ('POUTREMC_NGTA');

    --get Dirout
    vDirOUT    := STD.F_GETVALPAR('DIR-OUT');

    --get ENTORNO
    vEntorno:= STD.F_GETVALPAR('ENTORNO');
    if vEntorno = 'PROD' then    vTipFileMC := 'P'; end if;
    if vEntorno = 'CCAL' then    vTipFileMC := 'T'; end if;
    if vEntorno = 'DESA' then    vTipFileMC := 'T'; end if;

  IF pcodhcierre = '1' then
      dFecha := dFecha;
   ELSIF pcodhcierre = '2' then
      dFecha := dFecha + 1;
   END IF;

    -- Proceso para Banco Mercantil
    p_mainGen_byBank(psFecha,vFechac6,'BM',vIDProc,vDirOUT,pRepro,pcodhcierre,dfecha,vArchivo);
    if length(trim(vArchivo)) > 0 then
             vRetorno := vArchivo;
    end if;

    -- Reiniciando Variables
    vNumSecReg      := 0;
    vMontoTotal     := 0;
    vtotalMensaje   := 0;

    -- Proceso para Banco Provincial
    p_mainGen_byBank(psFecha,vFechac6,'BP',vIDProc,vDirOUT,pRepro,pcodhcierre,dfecha,vArchivo);
    if length(trim(vArchivo)) > 0 then
         vRetorno := vRetorno||vArchivo;
    end if;

    -- Verificacion de Warning
    if (vCantRegMer < 1 and vCantRegPro < 1) then
         verrmsg :='No hay registros para procesar en OUTGOING DE RETORNOS';
         verrcod := '21';
         raise eFinWarning;
    end if;

        /*--forzar valor a 0 de varibles
    vmontoTotal     := 0;
    vtotalMensaje   := 0;
    vCantRegMer     := 0;
    vCantRegPro     := 0;*/

    return '0'||vRetorno||'~';

exception
    when efinerror then
         utl_file.fclose(vIDFile);
         pqmonproc.inslog(vidproc, 'E', verrmsg);
         p_Insert_CtlProcMc(vIDProc,vbank4,'E',vSecControl,dfecha);
         vretc :=pqmonproc.updmonproc (vidproc, 'E');
         return 'EERROR: '||verrmsg||'~';

    when eFinWarning then
         utl_file.fclose(vIDFile);
         pqmonproc.inslog(vidproc, 'E', verrmsg);
         p_Insert_CtlProcMc(vIDProc,vbank4,'E',vSecControl,dfecha);
         vretc :=pqmonproc.updmonproc (vidproc, 'E');
         return 'WALERTA: '||verrmsg||'~';

    when others then
         voracode := abs(sqlcode);
         voraerr  := substr(sqlerrm,1,200);
         utl_file.fclose(vIDFile);
         pqmonproc.inslog(vidproc, 'E', voraerr);
         p_Insert_CtlProcMc(vIDProc,vbank4,'E',vSecControl,dfecha);
         vretc    := pqmonproc.updmonproc (vidproc, 'E', '99');
         RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';

end;





procedure p_mainGen_byBank(pFecha char,pFecha6 char,pBankChar char,
                           pIdProc number,pDirOut char,pRepro char:='N',
                           pcodhcierre CHAR,pfechad date, pfile out varchar) is
    vbank6          char(6)         := '';


begin

    if pBankChar = 'BM' then
       vbank4      := '0105';
       vbank6      := '540105';
       vCantRegMer := f_AtLeastOne(pFecha,vbank4);
    end if;

    if pBankChar = 'BP' then
       vbank4      := '0108';
       vbank6      := '540108';
       vCantRegpro := f_AtLeastOne(pFecha,vbank4);

    end if;

    --get_secuence
--    vSecControl  := f_getSecuencia(vbank4,'POUTREMC',pRepro,pfechad);
    vSecControl  := f_getSecuencia(pfechad);

    --inserta inicio de proceso
    p_Insert_CtlProcMc(pIdProc, vbank4,'I',vSecControl,pfechad);



    --validando directorio
    if pDirOut is null then
       verrmsg := 'Directorio de salida (dir_out) no definido en STD_PARAMETRO';
       verrcod := '2';
       raise efinerror;
    end if;

    --Validando si parametro de fecha es ok
    vb_isdate  := f_IsDate(pfecha);
    if vb_isDate = false then
       verrmsg := 'El parametro no es de tipo fecha [YYYYMMDD]';
       verrcod := '6';
      raise efinerror;
    end  if;




    --*********************************GEN FILE BANCO MERCANTIL**********************************
     if vCantRegMer > 0 and pBankChar = 'BM'   then
           --generando File
          vFile   := '';
--          vFile   := 'OUTMC'||vbank4||pFecha||lpad(to_char(vSecControl),2,0)||'.DAT';
          vFile   := 'OUTMC_NGTA'||vbank4||TO_CHAR(pfechad,'YYYYMMDD')||lpad(to_char(vSecControl),2,0)||'.DAT';
          pfile   := vfile;
          --validacion si archivo esta abierto
          vIDFile := utl_file.fopen(pDirOut,vFile,'w',32767);
          if not utl_file.is_open(vIDFile) then
            verrmsg := 'Al abrir <'||pDirOut||'/'||vFile||'>';
            verrcod := '99';
            raise efinerror;
          end if;

          --get_tipocambio
--          vTipocambio    := f_findTipoCambio(pbankChar,pFecha);

          --inicia proceso de outgoing
          vPaso   := 'Paso 02';
          pqmonproc.inslog(pIdProc, 'M', 'inicia Proceso de Outgoing Naiguata MasterCard BM');

          p_genReg1644(697,pFecha6,pBankChar);
          p_GenRegCompra(pFecha,vbank4,pcodhcierre); --pFecha
          p_GenRegDevolucion(pFecha,vbank4,pcodhcierre); --pFecha
          p_GenRegAnulacion(pFecha,vbank4,pcodhcierre); --pFecha
          p_GenReg1740(pFecha,vbank4); --pFecha
          p_genReg1644(695,pFecha6,pBankChar);
          utl_file.fCLOSE(vIDFile);

          pqmonproc.inslog(vidproc, 'M', 'fin ok | archivo: '||pDirOut||'/'||vFile);
          p_Insert_CtlProcMc(pIdProc, vbank4,'F',vSecControl,pfechad);
          vretc :=pqmonproc.updmonproc (pIdProc, 'F');
  end if;



  --*********************************GEN FILE BANCO PROVINCIAL**********************************
  if vCantRegPro > 0 and pBankChar = 'BP' then
          --generando File
          vFile   := '';
          --vFile   := 'OUTMC'||vbank4||pFecha||lpad(to_char(vSecControl),2,0)||'.DAT';
          vFile   := 'OUTMC'||vbank4||TO_CHAR(pfechad,'YYYYMMDD')||lpad(to_char(vSecControl),2,0)||'.DAT';
          pfile   := vfile;

          --validacion si archivo esta abierto
          vIDFile := utl_file.fopen(pDirOut,vFile,'w',32767);
          if not utl_file.is_open(vIDFile) then
            verrmsg := 'Al abrir <'||pDirOut||'/'||vFile||'>';
            verrcod := '99';
            raise efinerror;
          end if;

          --get_tipocambio
--          vTipocambio    := f_findTipoCambio(pbankChar,pFecha);

          --inicia proceso de outgoing
          vPaso   := 'Paso 02';
          pqmonproc.inslog(pIdProc, 'M', 'inicia Proceso de Outgoing MasterCard BM');

          p_genReg1644(697,pFecha6,pBankChar);
          p_GenRegCompra(pFecha,vbank4,pcodhcierre);
          p_GenRegDevolucion(pFecha,vbank4,pcodhcierre);
          p_GenRegAnulacion(pFecha,vbank4,pcodhcierre);
          p_GenReg1740(pFecha,vbank4);
          p_genReg1644(695,pFecha6,pBankChar);
          utl_file.fCLOSE(vIDFile);

          pqmonproc.inslog(vidproc, 'F', 'fin ok | archivo: '||pDirOut||'/'||vFile);
          p_Insert_CtlProcMc(pIdProc, vbank4,'F',vSecControl,pfechad);
          vretc :=pqmonproc.updmonproc (pIdProc, 'F');
  end if;

end;




procedure p_GenRegCompra(pfecha  varchar2, pbanco varchar2, pcodhcierre CHAR) is
    strhexa1240C       varchar2(32)     := '';
    vcodpostal         varchar2(10)     := '';
    vcodestado         varchar2(3)      := '';
    long_p48           number           := 0;
    vbanco             varchar2(2)      := '';
    vt1240C            msg_1240C;
    mb                 Mapa_Bits;
    vs                 s;
    vlong              varchar2(128);
    vdato              varchar2(1024);
    ncero              number := 0;
    pds0158_de48       VARCHAR2(20)    :='';
    mb40               VARCHAR2(1)     :='';
    vcodser            VARCHAR2(3)     :=NULL;
    wkDataGetIRD   VARCHAR2(200)     :=NULL;   /* TsT 04/12/2018 */

    videmen_p00_actc   clr_dcemvfull.idemen_p00_actc%TYPE;
    videtra_p11_actc   clr_dcemvfull.idetra_p11_actc%TYPE;
    vtimloc_p12_actc   clr_dcemvfull.timloc_p12_actc%TYPE;
    videadq_p32_actc   clr_dcemvfull.ideadq_p32_actc%TYPE;
    vemvtvr_p55_actc   clr_dcemvfull.emvtvr_p55_actc%TYPE;
    vperint_p55_actc   clr_dcemvfull.perint_p55_actc%TYPE;
    vnumale_p55_actc   clr_dcemvfull.numale_p55_actc%TYPE;
    vcapter_p55_actc   clr_dcemvfull.capter_p55_actc%TYPE;
    vscremi_p55_actc   clr_dcemvfull.scremi_p55_actc%TYPE;
    vnumsec_p23_actc   clr_dcemvfull.numsec_p23_actc%TYPE;
    vdaplem_p55_actc   clr_dcemvfull.daplem_p55_actc%TYPE;
    vttcrip_p55_actc   clr_dcemvfull.ttcrip_p55_actc%TYPE;
    vemvatc_p55_actc   clr_dcemvfull.emvatc_p55_actc%TYPE;
    vcrippe_p55_actc   clr_dcemvfull.crippe_p55_actc%TYPE;
    vpaicri_p55_actc   clr_dcemvfull.paicri_p55_actc%TYPE;
    vfeccri_p55_actc   clr_dcemvfull.feccri_p55_actc%TYPE;
    vimpcri_p55_actc   clr_dcemvfull.impcri_p55_actc%TYPE;
    vinfcri_p55_actc   clr_dcemvfull.infcri_p55_actc%TYPE;
    vmoncri_p55_actc   clr_dcemvfull.moncri_p55_actc%TYPE;
    vimccri_p55_actc   clr_dcemvfull.imccri_p55_actc%TYPE;

cursor cr_compra is
    select idemen_p00_actc,   lontar_l02_actc,   numtar_p02_actc,
           codpro_p03_actc,   imptra_p04_actc,   idetra_p11_actc, timloc_p12_actc,
           feccad_p14_actc,   punser_p22_actc,   codfun_p24_actc,
           codraz_p25_actc,   codact_p26_actc,   lpad(nvl(impori_p30_actc,0),12,0) impori_p30_actc,
           lonref_l31_actc,   refadq_p31_actc,   ideadq_l32_actc,
           ideadq_p32_actc,   idepre_l33_actc,   idepre_p33_actc,pista2_p35_actc,
           datref_p37_actc,   numaut_p38_actc,   ideter_p41_actc,
           ideest_p42_actc,   nomest_p43_actc,    locest_p43_actc,
           decode(paiest_p43_actc,'VE','VEN','VEN')paiest_p43_actc,
           pds0025_p48,
           londat_p48_actc,   montra_p49_actc,   moncon_p50_actc,CYCLID_P63_ACTC,
           nummen_p71_actc,   numemi_l95_actc,   numemi_p95_actc,
           sesion_p28_actc,   codser_p40_actc
          -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY')) idparticion /* implementaci¿n del particionamiento*/
         ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY'))||SUBSTR(sesion_p28_actc,3,2)||TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
          ,TO_NUMBER(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'MM')) id_mes
     from rechazos_retornos
    where trunc(fec_upd)          = to_date(pfecha,'yyyymmdd')
    and   SUBSTR(ideadq_p32_actc,3,4)  = pbanco
    and   idemen_p00_actc  = '1240'
    and   estado           = 'T'
    and   tipo_incoming    = 'NA'     --- IPR 1302 POR TIPO_INCOMING NAIGUATA -  "NA"
    and   oriide_p56_actc is null
   -- and   codpro_p03_actc  = '000000'
    and   pds0025_p48  is null;
   -- AND refadq_p31_actc IN('75502918285548719763247','75502918285548733890117'); -- TsT 21/11/2018
--    and   substr(tiptra_p48_actc,1,2)  = '06'
    --and   codfun_p24_actc  = '205'

 begin

   --inicia Generacion de Compra
   vPaso   := 'Paso 03';
   if pbanco = '0105' then vbanco :='BM'; end if;
   if pbanco = '0108' then vbanco :='BP'; end if;

   for c in cr_compra  loop
      long_p48:=0;

      vt1240C     := f_ini_msg1240C;

      mb:= f_inimapabits;
      -- presencia del mapa de bits secundario
      mb(1):='1';

      -- identificador de tipo de mensaje ('1240')
      vt1240C.idemen_p00_actc :=to_char(c.idemen_p00_actc);

      mb(2) :='1';     vt1240C.lontar_l02_actc := to_char(c.lontar_l02_actc);
                       vt1240C.numtar_p02_actc := c.numtar_p02_actc;

      mb(3) :='1';     vt1240C.codpro_p03_actc := lpad(c.codpro_p03_actc,6,0);

      mb(4) :='1';
      vt1240C.imptra_p04_actc := to_char(lpad(c.imptra_p04_actc,12,0));
                       vmontoTotal     := vmontoTotal + to_number(nvl(vt1240C.imptra_p04_actc,0));
                       vtotalMensaje   := vtotalMensaje + 1;

      mb(12):='1';     vt1240C.timloc_p12_actc := c.timloc_p12_actc;

      if c.feccad_p14_actc is not null then
         mb(14):='1';    vt1240C.feccad_p14_actc  := c.feccad_p14_actc;
      end if;

      mb(22):='1';
      if c.punser_p22_actc is null then
          vt1240C.punser_p22_actc := '000000000000';
      else
          vs.s1  := substr(c.punser_p22_actc,1,1);
          vs.s2  := substr(c.punser_p22_actc,2,1);
          vs.s3  := substr(c.punser_p22_actc,3,1);
          vs.s4  := substr(c.punser_p22_actc,4,1);
          IF  vs.s4 ='M' THEN -- realizado por Carlos Brito para: valida campo 22 subcampo 4, M es igual a mPOs y se modifica para que mande el valor 1 fecha 05/05/2017
              vs.s4:='1' ;
          END IF;
          vs.s6  := substr(c.punser_p22_actc,6,1);
          vs.s9  := substr(c.punser_p22_actc,9,1);
          vs.s10 := substr(c.punser_p22_actc,10,1);
          vs.s11 := substr(c.punser_p22_actc,11,1);
          vs.s12 := substr(c.punser_p22_actc,12,1);

          vs.s5 := substr(c.punser_p22_actc,5,1);
          if vs.s5 = '2' then    vs.s5 := '3';     end if;
          if vs.s5 = '3' then    vs.s5 := '4';     end if;
          if vs.s5 = '7' or      vs.s5 = '8'       then     vs.s5 := '5'; end if;

          vs.s7 := substr(c.punser_p22_actc,7,1);
          if vs.s7 = '5' then    vs.s7 := 'C';     end if;
          if vs.s7 = 'S' or      vs.s7  = 'T'      then     vs.s7 := 'B'; end if;
          if vs.s5 = '7' then    vs.s7 := 'T';     end if;
          if vs.s5 = '8' then    vs.s7 := 'S';     end if;

          vs.s8 := substr(c.punser_p22_actc,8,1);
          if vs.s8 = '8' then    vs.s8 := '6';     end if;
          --se concatena todas las cadenas
          vt1240C.punser_p22_actc := vs.s1||vs.s2||vs.s3||vs.s4||vs.s5||vs.s6||vs.s7||vs.s8||vs.s9||vs.s10||vs.s11||vs.s12;
       end if;

      mb(24):='1';   vt1240C.de_24   := c.codfun_p24_actc;

      mb(25):='1';   vt1240C.de_25   := lpad(c.codraz_p25_actc,4,0);
      mb(26):='1';   vt1240C.de_26   := lpad(c.codact_p26_actc,4,0);
      -- Solo para transacciones cuyo mcc = 7995(11/09/2009)
      if vt1240C.de_26 in ('4829','7995') then
         vt1240C.codpro_p03_actc := '180000';
      end if;

      mb(30):='1';
                     vt1240C.de_30s1  := to_char(lpad(c.imptra_p04_actc,12,0));
                     vt1240C.de_30s2  := '000000000000';
                     vt1240C.de_30    := vt1240C.de_30s1 || vt1240C.de_30s2;

      mb(31):='1';   vt1240C.de_31            :=  lpad(to_char(c.lonref_l31_actc),2,0)||lpad(c.refadq_p31_actc,23,0);

      mb(32):='1';
      if vbanco = 'BM' then  vt1240C.de_32 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_32 := '06010403'; end if;

--      mb(33):='1';   vt1240C.de_33            :=  lpad(to_char(c.idepre_l33_actc),2,0)||c.idepre_p33_actc;
      mb(33):='1';
      if vbanco = 'BM' then  vt1240C.de_33 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_33 := '06010403'; end if;

      mb(37):='1';   vt1240C.de_37            :=  rpad(c.datref_p37_actc,12,0);
      mb(38):='1';   vt1240C.de_38            :=  rpad(c.numaut_p38_actc,6,0);
      mb(41):='1';   vt1240C.de_41            :=  rpad(c.ideter_p41_actc,8,0);
      mb(42):='1';   vt1240C.de_42            :=  rpad(c.ideest_p42_actc,15,' ');
      mb(43):='1';   vt1240C.nomest_p43       :=  rpad(substr(c.nomest_p43_actc,1,22),22,' ')||'\';
                     vt1240C.dircomercio_p43  :=  '\';
                     vt1240C.locest_p43       :=  rpad(c.locest_p43_actc,13,' ')||'\';

                     p_findPostalCode(c.ideest_p42_actc,vcodpostal,vcodestado);
                     vcodpostal := pqutlcom.p_PostalCodexState(vcodestado); -- IPR 1080 CODIGO POSTAL POR ESTADO
                     vt1240C.cod_postal_p43   := rpad(vcodpostal,10,' ');
                     vt1240C.cod_estado_p43   := rpad(vcodestado,3,' ');
                     vt1240C.paiest_p43       := rpad(c.paiest_p43_actc,3,' ');
                     vt1240C.longdat_p43      := lpad(length(vt1240C.nomest_p43||
                                                        vt1240C.dircomercio_p43||
                                                        vt1240C.locest_p43||
                                                        vt1240C.cod_postal_p43||
                                                        vt1240C.cod_estado_p43||
                                                        vt1240C.paiest_p43),2,0);

                    vt1240C.de_43  :=  vt1240C.longdat_p43     || vt1240C.nomest_p43     ||
                                       vt1240C.dircomercio_p43 || vt1240C.locest_p43     ||
                                       vt1240C.cod_postal_p43  || vt1240C.cod_estado_p43 ||
                                       vt1240C.paiest_p43;

      mb(48):='1';
      --de48_pds0025
      if upper(substr(c.pds0025_p48,1,1)) = 'R' then
         vt1240C.pds0025_de48 := '0025007R'||substr(c.pds0025_p48,2,6);
      end if;
      --de48_pds0148
      --vt1240C.isnum_nacional := f_FindTarjetaNacional(c.numtar_p02_actc,vbanco);
      --if vt1240C.isnum_nacional > 0 then
         vt1240C.pds0148_de48   := '0023003NA 0148004'||c.montra_p49_actc||'2';
      --else
      --   vt1240C.pds0148_de48   := '0023003NA 01480048402';
      --end if;
      --de48_pds0149
      vt1240C.pds0149_de48   := '0149006'||c.montra_p49_actc||'000';
      ----de48_pds0158
      if c.codfun_p24_actc != '200'  then
         p_findTransactionIdDisputas(c.refadq_p31_actc, c.numtar_p02_actc,vt1240C.TransationID,vt1240C.SourceAmount,vt1240C.currenCode,vt1240C.pds0262_Ide48);
         vt1240C.pds0262_de48   := '0262001'||vt1240C.pds0262_Ide48;
      end if;

      PQOUTRETORNOMC_NGTA.p_findird(vt1240C.de_26, vt1240C.cod_mcc, vt1240C.cod_ird, vt1240C.cod_tcc );

      --if vt1240C.isnum_nacional > 0 then     --Tarjetas Nacionales

         if  to_number(vt1240C.de_26)  = to_number(vt1240C.cod_mcc)  then
             vt1240C.pds0158_de48 := '0158012MCC       '||lpad(to_char(vt1240C.cod_ird),2,'0');
         end if;

      /*else                                    --Tarjetas Corporativas

         --p_getDataInternacional(c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region);
         pqoutgoingmc.p_getDataInternacional(vIDProc,c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region,vt1240C.cod_programa);
         --Para efecto de manteniento y para el proyecto Naiguata no se redirecciona a los nuevos paquetes pqoutgoingmc_ngta irp 1302
         vt1240C.isBetweenFiveday := f_isBetweenFiveday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));
         vt1240C.isBetweenThirtyday := f_isBetweenThirtyday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));

         vcodser := LPAD(NVL(c.codser_p40_actc,'0'),3,'0');

         vt1240C.FecPresentacion := SUBSTR(c.TIMLOC_P12_ACTC,1,6);

         if vt1240C.Cod_Producto_GCMS in ('DAG','DAP','DAS','DOS','SAG','SAP','SAS','SOS','WBE','MBP','MBT','MTP','MDJ','MRH') then
            vt1240C.pds0002_de48 := vt1240C.Cod_Producto_GCMS ;
            vt1240C.pds0003_de48 := vt1240C.cod_licen_producto;
         else
            vt1240C.pds0002_de48 := '';
            vt1240C.pds0003_de48 := '';
         end if;

         pqoutgoingmc.p_Obtiene_IRD(vt1240C.Cod_Producto_GCMS,vt1240C.cod_region,c.punser_p22_actc,vt1240C.punser_p22_actc,vcodser,vt1240C.isBetweenFiveday,vt1240C.isBetweenThirtyday,vt1240C.FecPresentacion,vt1240C.cod_programa,pds0158_de48,mb40);
         --ipr 1302 Para efecto de manteniento y para el proyecto Naiguata no se redirecciona a los nuevos paquetes pqoutgoingmc_ngta irp 1302 
         vt1240C.pds0158_de48 := pds0158_de48;

         IF mb40 = '1' THEN
            mb(40):= mb40;
            vt1240C.de_40 := vcodser;
         END IF;


      END IF;*/

      vt1240C.pds0165_de48 := '0165001M';

      vt1240C.De48         := vt1240C.pds0025_de48||  vt1240C.pds0148_de48||
                              vt1240C.pds0149_de48||  vt1240C.pds0158_de48||
                              vt1240C.pds0165_de48||  vt1240C.pds0262_de48;
      vt1240C.londat_P48 := Lpad(length(vt1240C.De48),3,'0');

      if SUBSTR(c.punser_p22_actc,7,1) = '5' then

         select emvtvr_p55_actc, perint_p55_actc, numale_p55_actc, capter_p55_actc,
                scremi_p55_actc, numsec_p23_actc, daplem_p55_actc, ttcrip_p55_actc,
                infcri_p55_actc, emvatc_p55_actc, crippe_p55_actc, paicri_p55_actc,
                feccri_p55_actc, impcri_p55_actc, moncri_p55_actc, imccri_p55_actc
           into vemvtvr_p55_actc, vperint_p55_actc, vnumale_p55_actc, vcapter_p55_actc,
                vscremi_p55_actc, vnumsec_p23_actc, vdaplem_p55_actc, vttcrip_p55_actc,
                vinfcri_p55_actc, vemvatc_p55_actc, vcrippe_p55_actc, vpaicri_p55_actc,
                vfeccri_p55_actc, vimpcri_p55_actc, vmoncri_p55_actc, vimccri_p55_actc
           from CLR_DCEMVFULL
          where idemen_p00_actc = DECODE(c.idemen_p00_actc,1240,1244,1440,1444)
            and idetra_p11_actc = c.idetra_p11_actc
            and timloc_p12_actc = c.timloc_p12_actc
            and ideadq_p32_actc = c.ideadq_p32_actc
            and idparticion =c.idparticion /* implementaci¿n del particionamiento*/
            and id_mes      =c.id_mes;

--         mb(23):='1';
--         vt1240C.numsec_p23_actc := LPAD(NVL(TRIM(vnumsec_p23_actc),'0'),3,'0');

--         if vnumsec_p23_actc is not null then
            mb(23):='1';
            vt1240C.numsec_p23_actc := LPAD(NVL(vnumsec_p23_actc,'0'),3,'0');
--         end if;

         mb(55):='1';

--         if vcrippe_p55_actc is null then
--            vt1240C.t9f26_de55 := '';
--         else
            vdato := LPAD(NVL(vcrippe_p55_actc,'0'),16,'0'); --std.PF_BCD2HEX(vcrippe_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vcrippe_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f26_de55 := '9F26'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vinfcri_p55_actc is null then
--            vt1240C.t9f27_de55 := '';
--         else
            vdato := LPAD(NVL(vinfcri_p55_actc,'0'),2,'0'); --std.PF_BCD2HEX(vcrippe_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vinfcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f27_de55 := '9F27'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F27')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vdaplem_p55_actc is null then
--            vt1240C.t9f10_de55 := '';
--         else
            vdato := TRIM(PQOUTGOINGMC.f_QUITA_20(vdaplem_p55_actc)); --,'0'),64,'0'); --std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)); --std.pf_hex2bin(std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f10_de55 := '9F10'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F10')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vnumale_p55_actc is null then
--            vt1240C.t9f37_de55 := '';
--         else
            vdato := LPAD(NVL(vnumale_p55_actc,'0'),8,'0'); --std.PF_BCD2HEX(vnumale_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vnumale_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f37_de55 := '9F37'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F37')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vemvatc_p55_actc is null then
--            vt1240C.t9f36_de55 := '';
--         else
            vdato := LPAD(NVL(vemvatc_p55_actc,'0'),4,'0'); --std.PF_BCD2HEX(vemvatc_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvatc_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f36_de55 := '9F36'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F36')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vemvtvr_p55_actc is null then
--            vt1240C.t95_de55 := '';
--         else
            vdato := LPAD(NVL(vemvtvr_p55_actc,'0'),10,'0'); --std.PF_BCD2HEX(vemvtvr_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvtvr_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t95_de55 := '95'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('95')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vfeccri_p55_actc is null then
--            vt1240C.t9a_de55 := '';
--         else
            vdato := LPAD(NVL(vfeccri_p55_actc,'0'),6,'0'); --std.pf_bcd2hex(vfeccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vfeccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9a_de55 := '9A'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9A')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vttcrip_p55_actc is null then
--            vt1240C.t9c_de55 := '';
--         else
            vdato := LPAD(NVL(vttcrip_p55_actc,'0'),2,'0'); --std.pf_bcd2hex(vttcrip_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vttcrip_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9c_de55 := '9C'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9C')||vlong||vdato;
            vlong := '';
            vdato := '';
---         end if;

--         if vimpcri_p55_actc is null then
--            vt1240C.t9f02_de55 := '';
--         else
            vdato := LPAD(NVL(vimpcri_p55_actc,'0'),12,'0'); --std.pf_bcd2hex(vimpcri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimpcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f02_de55 := '9F02'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F02')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vmoncri_p55_actc is null then
--            vt1240C.t5f2a_de55 := '';
--         else
            vdato := LPAD(NVL(vmoncri_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vmoncri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vmoncri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t5f2a_de55 := '5F2A'||lpad(vlong,2,'0')||vdato; ----std.pf_hex2bin('5F2A')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vperint_p55_actc is null then
--            vt1240C.t82_de55 := '';
--         else
            vdato := LPAD(NVL(vperint_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vperint_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vperint_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8))
            vt1240C.t82_de55 := '82'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vpaicri_p55_actc is null then
--            vt1240C.t9f1a_de55 := '';
--         else
            vdato := LPAD(NVL(vpaicri_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vpaicri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vpaicri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f1a_de55 := '9F1A'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vimccri_p55_actc is null then
--            vt1240C.t9f03_de55 := '';
--         else
            vdato := LPAD(NVL(vimccri_p55_actc,'0'),12,'0'); --std.pf_bcd2hex(vimccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f03_de55 := '9F03'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F03')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

         vt1240C.De55      := vt1240C.t9f26_de55||vt1240C.t9f27_de55||vt1240C.t9f10_de55||vt1240C.t9f37_de55||
                              vt1240C.t9f36_de55||vt1240C.t95_de55  ||vt1240C.t9a_de55  ||vt1240C.t9c_de55  ||
                              vt1240C.t9f02_de55||vt1240C.t5f2a_de55||vt1240C.t82_de55  ||vt1240C.t9f1a_de55||
                              vt1240C.t9f03_de55;

         -- JMG: valida si P55 es nulo
         IF vt1240C.De55 IS NULL THEN
            mb(55):='0';
            pqmonproc.inslog(vidproc, 'W', 'No Existe P55 para P31='||vt1240C.de_31);
         ELSE
            vt1240C.londat_p55 := lpad(length(vt1240C.De55)/2,3,'0');
         END IF;
--         vt1240C.londat_p55 := lpad(std.pf_dec2hex(length(vt1240C.De55)/2),3,'0');

      end if;

      mb(49):='1';     vt1240C.de_49 := c.montra_p49_actc;

      if nvl(c.CYCLID_P63_ACTC,' ') <> ' ' then
        mb(63):='1';     vt1240C.de_63 := '016 ' || rpad(substr(c.CYCLID_P63_ACTC,2,15),15,' ');
      end if;

      mb(71):='1';     vNumSecReg    := vNumSecReg + 1;
                       vt1240C.de_71 := lpad(vNumSecReg,8,0);

      if c.codfun_p24_actc != '200' then
         if vt1240C.de_25 in ('2001','2002','2003','2004','2005','2008','2011',
                              '2700','2701','2702','2703','2704','2705','2706',
                              '2707','2708','2709','2710','2713','2870','2871') and length(trim(c.numemi_p95_actc)) > 0  then
            mb(72):='1';
            vt1240C.londat_de72     := to_char(lpad(length(trim(substr(c.numemi_p95_actc,7,50))),3,0)); --to_char(lpad(length(c.numemi_p95_actc),3,0));
            vt1240C.de_72           := trim(substr(c.numemi_p95_actc,7,50)); --substr(c.numemi_p95_actc,7,50);
         end if;
      end if;

      mb(94):='1';     vt1240C.de_94 := vt1240C.de_33;
--      mb(95):='1';     vt1240C.de_95 := '10'||substr(c.numemi_p95_actc,1,10);

       if c.codfun_p24_actc != '200'  then
         mb(95):='1';   vt1240C.de_95 := '10'||vt1240C.TransationID;
      end if;

      -- Concatenar los campos del registro en linea
      strHexa1240C:= rtrim(ltrim(f_gen_MapaHex2(mb)));
      vt1240C.bitpri_actc := substr(strHexa1240C,1,16);
      vt1240C.bitsec_actc := substr(strHexa1240C,17,16);

      vcad1240:= vt1240C.idemen_p00_actc || vt1240C.bitpri_actc      || vt1240C.bitsec_actc      ||
                 vt1240C.lontar_l02_actc || vt1240C.numtar_p02_actc  || vt1240C.codpro_p03_actc  ||
                 vt1240C.imptra_p04_actc || vt1240C.timloc_p12_actc  || vt1240C.feccad_p14_actc  ||
                 vt1240C.punser_p22_actc || vt1240C.numsec_p23_actc  || vt1240C.de_24            || vt1240C.de_25  ||
                 vt1240C.de_26           || vt1240C.de_30            || vt1240C.de_31  ||
                 vt1240C.de_32           ||
                 vt1240C.de_33           || vt1240C.de_37            || vt1240C.de_38  || vt1240C.de_40  ||
                 vt1240C.de_41           || vt1240C.de_42            || vt1240C.de_43  ||
                 vt1240C.londat_P48      || vt1240C.De48             || vt1240C.de_49  ||
                 vt1240C.moncon_p50_actc || vt1240C.londat_p55       || vt1240C.De55   ||
                 vt1240C.de_63           || vt1240C.de_71            ||
                 vt1240C.londat_de72     || vt1240C.de_72            || vt1240C.de_94  ||
                 vt1240C.de_95;
      --formato fijo a 1024 posiciones
      --vlinea   := rpad(vlinea,1024,' ');

      utl_file.put_raw(vidfile, utl_raw.cast_to_raw(vcad1240),true);
      utl_file.new_line(vidfile);
      utl_file.fflush(vidfile);

      -- Inicio: Calculos para Registro de Conciliacion (1240)
      vMtoTot_1240.TotalMensaje  := vMtoTot_1240.TotalMensaje + 1;
end loop;

end;








procedure p_GenRegDevolucion(pfecha  varchar2, pbanco varchar2,pcodhcierre CHAR) is
    strhexa1240C       varchar2(32)     := '';
    vcodpostal         varchar2(10)     := '';
    vcodestado         varchar2(3)      := '';
    long_p48           number           := 0;
    vbanco             varchar2(2)      := '';
    vt1240C            msg_1240C;
    mb                 Mapa_Bits;
    vs                 s;
    vlong              varchar2(128);
    vdato              varchar2(1024);
    ncero              number := 0;
    pds0158_de48       VARCHAR2(20)    :='';
    mb40               VARCHAR2(1)     :='';
    vcodser            VARCHAR2(3)     :=NULL;
    videmen_p00_actc   clr_dcemvfull.idemen_p00_actc%TYPE;
    videtra_p11_actc   clr_dcemvfull.idetra_p11_actc%TYPE;
    vtimloc_p12_actc   clr_dcemvfull.timloc_p12_actc%TYPE;
    videadq_p32_actc   clr_dcemvfull.ideadq_p32_actc%TYPE;
    vemvtvr_p55_actc   clr_dcemvfull.emvtvr_p55_actc%TYPE;
    vperint_p55_actc   clr_dcemvfull.perint_p55_actc%TYPE;
    vnumale_p55_actc   clr_dcemvfull.numale_p55_actc%TYPE;
    vcapter_p55_actc   clr_dcemvfull.capter_p55_actc%TYPE;
    vscremi_p55_actc   clr_dcemvfull.scremi_p55_actc%TYPE;
    vnumsec_p23_actc   clr_dcemvfull.numsec_p23_actc%TYPE;
    vdaplem_p55_actc   clr_dcemvfull.daplem_p55_actc%TYPE;
    vttcrip_p55_actc   clr_dcemvfull.ttcrip_p55_actc%TYPE;
    vemvatc_p55_actc   clr_dcemvfull.emvatc_p55_actc%TYPE;
    vcrippe_p55_actc   clr_dcemvfull.crippe_p55_actc%TYPE;
    vpaicri_p55_actc   clr_dcemvfull.paicri_p55_actc%TYPE;
    vfeccri_p55_actc   clr_dcemvfull.feccri_p55_actc%TYPE;
    vimpcri_p55_actc   clr_dcemvfull.impcri_p55_actc%TYPE;
    vinfcri_p55_actc   clr_dcemvfull.infcri_p55_actc%TYPE;
    vmoncri_p55_actc   clr_dcemvfull.moncri_p55_actc%TYPE;
    vimccri_p55_actc   clr_dcemvfull.imccri_p55_actc%TYPE;



cursor cr_Devolucion is
    select idemen_p00_actc,   lontar_l02_actc,   numtar_p02_actc,
           codpro_p03_actc,   imptra_p04_actc,   idetra_p11_actc, timloc_p12_actc,
           feccad_p14_actc,   punser_p22_actc,   codfun_p24_actc,
           codraz_p25_actc,   codact_p26_actc,   lpad(nvl(impori_p30_actc,0),12,0) impori_p30_actc,
           lonref_l31_actc,   refadq_p31_actc,   ideadq_l32_actc,
           ideadq_p32_actc,   idepre_l33_actc,   idepre_p33_actc,pista2_p35_actc,
           datref_p37_actc,   numaut_p38_actc,   ideter_p41_actc,
           ideest_p42_actc,   nomest_p43_actc,    locest_p43_actc,
           decode(paiest_p43_actc,'VE','VEN','VEN')paiest_p43_actc,
           pds0025_p48,
           londat_p48_actc,   montra_p49_actc,   moncon_p50_actc,CYCLID_P63_ACTC,
           nummen_p71_actc,   numemi_l95_actc,   numemi_p95_actc,
           sesion_p28_actc,   codser_p40_actc
          -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY')) idparticion /* Implementaci¿n del particionamiento */
          ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY'))||SUBSTR(sesion_p28_actc,3,2)||TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
          ,TO_NUMBER(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'MM')) id_mes
     from rechazos_retornos
    where trunc(fec_upd)          = to_date(pfecha,'yyyymmdd')
    and   SUBSTR(ideadq_p32_actc,3,4)  = pbanco
    and   idemen_p00_actc  = '1240'
    and   estado           = 'T'
    and   tipo_incoming    = 'NA'      --ipr 1302 tipo_incoming para naiguata
--    and   codfun_p24_actc  = '205'
    and   codpro_p03_actc  = '200000';


 begin

   --inicia Generacion de Compra
   vPaso   := 'Paso 04';
   if pbanco = '0105' then vbanco :='BM'; end if;
   if pbanco = '0108' then vbanco :='BP'; end if;

   for c in cr_Devolucion  loop
      long_p48:=0;

      vt1240C     := f_ini_msg1240C;

      mb:= f_inimapabits;
      -- presencia del mapa de bits secundario
      mb(1):='1';

      -- identificador de tipo de mensaje ('1240')
      vt1240C.idemen_p00_actc :=to_char(c.idemen_p00_actc);

      mb(2) :='1';     vt1240C.lontar_l02_actc := to_char(c.lontar_l02_actc);
                       vt1240C.numtar_p02_actc := c.numtar_p02_actc;

      mb(3) :='1';     vt1240C.codpro_p03_actc := lpad(c.codpro_p03_actc,6,0);

      mb(4) :='1';     vt1240C.imptra_p04_actc := to_char(lpad(c.imptra_p04_actc,12,0));
                       vmontoTotal   := vmontoTotal + to_number(nvl(vt1240C.imptra_p04_actc,0));
                       vtotalMensaje := vtotalMensaje + 1;

      mb(12):='1';     vt1240C.timloc_p12_actc := c.timloc_p12_actc;

      if c.feccad_p14_actc is not null then
         mb(14):='1';    vt1240C.feccad_p14_actc  := c.feccad_p14_actc;
      end if;

      mb(22):='1';
      if c.punser_p22_actc is null then
          vt1240C.punser_p22_actc := '000000000000';
      else
          vs.s1  := substr(c.punser_p22_actc,1,1);
          vs.s2  := substr(c.punser_p22_actc,2,1);
          vs.s3  := substr(c.punser_p22_actc,3,1);
          vs.s4  := substr(c.punser_p22_actc,4,1);
          IF  vs.s4 ='M' THEN -- realizado por Carlos Brito para: valida campo 22 subcampo 4, M es igual a mPOs y se modifica para que mande el valor 1 fecha 05/05/2017
              vs.s4:='1' ;
          END IF;
          vs.s6  := substr(c.punser_p22_actc,6,1);
          vs.s9  := substr(c.punser_p22_actc,9,1);
          vs.s10 := substr(c.punser_p22_actc,10,1);
          vs.s11 := substr(c.punser_p22_actc,11,1);
          vs.s12 := substr(c.punser_p22_actc,12,1);

          vs.s5 := substr(c.punser_p22_actc,5,1);
          if vs.s5 = '2' then    vs.s5 := '3';     end if;
          if vs.s5 = '3' then    vs.s5 := '4';     end if;
          if vs.s5 = '7' or      vs.s5 = '8'       then     vs.s5 := '5'; end if;

          vs.s7 := substr(c.punser_p22_actc,7,1);
          if vs.s7 = '5' then    vs.s7 := 'C';     end if;
          if vs.s7 = 'S' or      vs.s7  = 'T'      then     vs.s7 := 'B'; end if;
          if vs.s5 = '7' then    vs.s7 := 'T';     end if;
          if vs.s5 = '8' then    vs.s7 := 'S';     end if;

          vs.s8 := substr(c.punser_p22_actc,8,1);
          if vs.s8 = '8' then    vs.s8 := '6';     end if;
          --se concatena todas las cadenas
          vt1240C.punser_p22_actc := vs.s1||vs.s2||vs.s3||vs.s4||vs.s5||vs.s6||vs.s7||vs.s8||vs.s9||vs.s10||vs.s11||vs.s12;
       end if;

      mb(24):='1';   vt1240C.de_24   := c.codfun_p24_actc;
      mb(25):='1';   vt1240C.de_25   := lpad(c.codraz_p25_actc,4,0);
      mb(26):='1';   vt1240C.de_26   := lpad(c.codact_p26_actc,4,0);
      -- Solo para transacciones cuyo mcc = 7995(11/09/2009)
      if vt1240C.de_26 in ('4829','7995') then
         vt1240C.codpro_p03_actc := '180000';
      end if;

      mb(30):='1';
                     vt1240C.de_30s1  := to_char(lpad(c.imptra_p04_actc,12,0));
                     vt1240C.de_30s2  := '000000000000';
                     vt1240C.de_30    := vt1240C.de_30s1 || vt1240C.de_30s2;

      mb(31):='1';   vt1240C.de_31            :=  lpad(to_char(c.lonref_l31_actc),2,0)||lpad(c.refadq_p31_actc,23,0);

/*      mb(32):='1';
      if vbanco = 'BM' then  vt1240C.de_32 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_32 := '06010403'; end if;

      mb(33):='1';   vt1240C.de_33            :=  lpad(to_char(c.idepre_l33_actc),2,0)||c.idepre_p33_actc;*/

      mb(32):='1';
      if vbanco = 'BM' then  vt1240C.de_32 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_32 := '06010403'; end if;

--      mb(33):='1';   vt1240C.de_33            :=  lpad(to_char(c.idepre_l33_actc),2,0)||c.idepre_p33_actc;
      mb(33):='1';
      if vbanco = 'BM' then  vt1240C.de_33 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_33 := '06010403'; end if;

      mb(37):='1';   vt1240C.de_37            :=  rpad(c.datref_p37_actc,12,0);
      mb(38):='1';   vt1240C.de_38            :=  rpad(c.numaut_p38_actc,6,0);
      mb(41):='1';   vt1240C.de_41            :=  rpad(c.ideter_p41_actc,8,0);
      mb(42):='1';   vt1240C.de_42            :=  rpad(c.ideest_p42_actc,15,' ');
      mb(43):='1';   vt1240C.nomest_p43       :=  rpad(substr(c.nomest_p43_actc,1,22),22,' ')||'\';
                     vt1240C.dircomercio_p43  :=  '\';
                     vt1240C.locest_p43       :=  rpad(c.locest_p43_actc,13,' ')||'\';

                     p_findPostalCode(c.ideest_p42_actc,vcodpostal,vcodestado);
                     vcodpostal := pqutlcom.p_PostalCodexState(vcodestado); -- IPR 1080 CODIGO POSTAL POR ESTADO
                     vt1240C.cod_postal_p43   := rpad(vcodpostal,10,' ');
                     vt1240C.cod_estado_p43   := rpad(vcodestado,3,' ');
                     vt1240C.paiest_p43       := rpad(c.paiest_p43_actc,3,' ');
                     vt1240C.longdat_p43      := lpad(length(vt1240C.nomest_p43||
                                                        vt1240C.dircomercio_p43||
                                                        vt1240C.locest_p43||
                                                        vt1240C.cod_postal_p43||
                                                        vt1240C.cod_estado_p43||
                                                        vt1240C.paiest_p43),2,0);

                    vt1240C.de_43  :=  vt1240C.longdat_p43     || vt1240C.nomest_p43     ||
                                       vt1240C.dircomercio_p43 || vt1240C.locest_p43     ||
                                       vt1240C.cod_postal_p43  || vt1240C.cod_estado_p43 ||
                                       vt1240C.paiest_p43;

      mb(48):='1';
      --de48_pds0025
      if upper(substr(c.pds0025_p48,1,1)) = 'R' then
         vt1240C.pds0025_de48 := '0025007R'||substr(c.pds0025_p48,2,6);
      end if;
      --de48_pds0148
      --vt1240C.isnum_nacional := f_FindTarjetaNacional(c.numtar_p02_actc,vbanco);
      --if vt1240C.isnum_nacional > 0 then
         vt1240C.pds0148_de48   := '0023003NA 0148004'||c.montra_p49_actc||'2';
      --else
      --   vt1240C.pds0148_de48   := '0023003NA 01480048402';
      --end if;
      --de48_pds0149
      vt1240C.pds0149_de48   := '0149006'||c.montra_p49_actc||'000';
      --de48_pds0158

      if c.codfun_p24_actc != '200'  then
         p_findTransactionIdDisputas(c.refadq_p31_actc, c.numtar_p02_actc,vt1240C.TransationID,vt1240C.SourceAmount,vt1240C.currenCode,vt1240C.pds0262_Ide48);
         vt1240C.pds0262_de48   := '0262001'||vt1240C.pds0262_Ide48;
      end if;

      p_findird(vt1240C.de_26, vt1240C.cod_mcc, vt1240C.cod_ird, vt1240C.cod_tcc );

      --if vt1240C.isnum_nacional > 0 then     --Tarjetas Nacionales

         if  to_number(vt1240C.de_26)  = to_number(vt1240C.cod_mcc)  then
             vt1240C.pds0158_de48 := '0158012MCC       '||lpad(to_char(vt1240C.cod_ird),2,'0');
         end if;

      /*else                                    --Tarjetas Corporativas

         --p_getDataInternacional(c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region);
         pqoutgoingmc.p_getDataInternacional(vIDProc,c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region,vt1240C.cod_programa);
         vt1240C.isBetweenFiveday := f_isBetweenFiveday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));
         vt1240C.isBetweenThirtyday := f_isBetweenThirtyday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));

         vcodser := LPAD(NVL(c.codser_p40_actc,'0'),3,'0');

         vt1240C.FecPresentacion := SUBSTR(c.TIMLOC_P12_ACTC,1,6);

         if vt1240C.Cod_Producto_GCMS in ('DAG','DAP','DAS','DOS','SAG','SAP','SAS','SOS','WBE','MBP','MBT','MTP','MDJ','MRH') then
            vt1240C.pds0002_de48 := vt1240C.Cod_Producto_GCMS ;
            vt1240C.pds0003_de48 := vt1240C.cod_licen_producto;
         else
            vt1240C.pds0002_de48 := '';
            vt1240C.pds0003_de48 := '';
         end if;

         pqoutgoingmc.p_Obtiene_IRD(vt1240C.Cod_Producto_GCMS,vt1240C.cod_region,c.punser_p22_actc,vt1240C.punser_p22_actc,vcodser,vt1240C.isBetweenFiveday,vt1240C.isBetweenThirtyday,vt1240C.FecPresentacion,vt1240C.cod_programa,pds0158_de48,mb40);

         vt1240C.pds0158_de48 := pds0158_de48;

         IF mb40 = '1' THEN
            mb(40):= mb40;
            vt1240C.de_40 := vcodser;
         END IF;

      END IF;*/

      vt1240C.pds0165_de48 := '0165001M';

      vt1240C.De48         := vt1240C.pds0025_de48||  vt1240C.pds0148_de48||
                              vt1240C.pds0149_de48||  vt1240C.pds0158_de48||
                              vt1240C.pds0165_de48||  vt1240C.pds0262_de48;

      vt1240C.londat_P48 := Lpad(length(vt1240C.De48),3,'0');

      if SUBSTR(c.punser_p22_actc,7,1) = '5' then

         select emvtvr_p55_actc, perint_p55_actc, numale_p55_actc, capter_p55_actc,
                scremi_p55_actc, numsec_p23_actc, daplem_p55_actc, ttcrip_p55_actc,
                infcri_p55_actc, emvatc_p55_actc, crippe_p55_actc, paicri_p55_actc,
                feccri_p55_actc, impcri_p55_actc, moncri_p55_actc, imccri_p55_actc
           into vemvtvr_p55_actc, vperint_p55_actc, vnumale_p55_actc, vcapter_p55_actc,
                vscremi_p55_actc, vnumsec_p23_actc, vdaplem_p55_actc, vttcrip_p55_actc,
                vinfcri_p55_actc, vemvatc_p55_actc, vcrippe_p55_actc, vpaicri_p55_actc,
                vfeccri_p55_actc, vimpcri_p55_actc, vmoncri_p55_actc, vimccri_p55_actc
           from CLR_DCEMVFULL
          where idemen_p00_actc = DECODE(c.idemen_p00_actc,1240,1244,1440,1444)
            and idetra_p11_actc = c.idetra_p11_actc
            and timloc_p12_actc = c.timloc_p12_actc
            and ideadq_p32_actc = c.ideadq_p32_actc
            and idparticion =c.idparticion /* Implementaci¿n del particionamiento */
            and id_mes      =c.id_mes    /* Implementaci¿n del particionamiento */;

--         mb(23):='1';
--         vt1240C.numsec_p23_actc := LPAD(NVL(TRIM(vnumsec_p23_actc),'0'),3,'0');

--         if vnumsec_p23_actc is not null then
            mb(23):='1';
            vt1240C.numsec_p23_actc := LPAD(NVL(vnumsec_p23_actc,'0'),3,'0');
--         end if;

         mb(55):='1';

--         if vcrippe_p55_actc is null then
--            vt1240C.t9f26_de55 := '';
--         else
            vdato := LPAD(NVL(vcrippe_p55_actc,'0'),16,'0'); --std.PF_BCD2HEX(vcrippe_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vcrippe_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f26_de55 := '9F26'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vinfcri_p55_actc is null then
--            vt1240C.t9f27_de55 := '';
--         else
            vdato := LPAD(NVL(vinfcri_p55_actc,'0'),2,'0'); --std.PF_BCD2HEX(vcrippe_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vinfcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f27_de55 := '9F27'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F27')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vdaplem_p55_actc is null then
--            vt1240C.t9f10_de55 := '';
--         else
            vdato := TRIM(PQOUTGOINGMC.f_QUITA_20(vdaplem_p55_actc)); --,'0'),64,'0'); --std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)); --std.pf_hex2bin(std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f10_de55 := '9F10'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F10')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vnumale_p55_actc is null then
--            vt1240C.t9f37_de55 := '';
--         else
            vdato := LPAD(NVL(vnumale_p55_actc,'0'),8,'0'); --std.PF_BCD2HEX(vnumale_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vnumale_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f37_de55 := '9F37'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F37')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vemvatc_p55_actc is null then
--            vt1240C.t9f36_de55 := '';
--         else
            vdato := LPAD(NVL(vemvatc_p55_actc,'0'),4,'0'); --std.PF_BCD2HEX(vemvatc_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvatc_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f36_de55 := '9F36'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F36')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vemvtvr_p55_actc is null then
--            vt1240C.t95_de55 := '';
--         else
            vdato := LPAD(NVL(vemvtvr_p55_actc,'0'),10,'0'); --std.PF_BCD2HEX(vemvtvr_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvtvr_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t95_de55 := '95'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('95')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vfeccri_p55_actc is null then
--            vt1240C.t9a_de55 := '';
--         else
            vdato := LPAD(NVL(vfeccri_p55_actc,'0'),6,'0'); --std.pf_bcd2hex(vfeccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vfeccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9a_de55 := '9A'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9A')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vttcrip_p55_actc is null then
--            vt1240C.t9c_de55 := '';
--         else
            vdato := LPAD(NVL(vttcrip_p55_actc,'0'),2,'0'); --std.pf_bcd2hex(vttcrip_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vttcrip_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9c_de55 := '9C'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9C')||vlong||vdato;
            vlong := '';
            vdato := '';
---         end if;

--         if vimpcri_p55_actc is null then
--            vt1240C.t9f02_de55 := '';
--         else
            vdato := LPAD(NVL(vimpcri_p55_actc,'0'),12,'0'); --std.pf_bcd2hex(vimpcri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimpcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f02_de55 := '9F02'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F02')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vmoncri_p55_actc is null then
--            vt1240C.t5f2a_de55 := '';
--         else
            vdato := LPAD(NVL(vmoncri_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vmoncri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vmoncri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t5f2a_de55 := '5F2A'||lpad(vlong,2,'0')||vdato; ----std.pf_hex2bin('5F2A')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vperint_p55_actc is null then
--            vt1240C.t82_de55 := '';
--         else
            vdato := LPAD(NVL(vperint_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vperint_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vperint_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8))
            vt1240C.t82_de55 := '82'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vpaicri_p55_actc is null then
--            vt1240C.t9f1a_de55 := '';
--         else
            vdato := LPAD(NVL(vpaicri_p55_actc,'0'),4,'0'); --std.pf_bcd2hex(vpaicri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vpaicri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f1a_de55 := '9F1A'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--         end if;

--         if vimccri_p55_actc is null then
--            vt1240C.t9f03_de55 := '';
--         else
            vdato := LPAD(NVL(vimccri_p55_actc,'0'),12,'0'); --std.pf_bcd2hex(vimccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f03_de55 := '9F03'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F03')||vlong||vdato;
            vlong := '';
            vdato := '';
--         end if;

         vt1240C.De55      := vt1240C.t9f26_de55||vt1240C.t9f27_de55||vt1240C.t9f10_de55||vt1240C.t9f37_de55||
                              vt1240C.t9f36_de55||vt1240C.t95_de55  ||vt1240C.t9a_de55  ||vt1240C.t9c_de55  ||
                              vt1240C.t9f02_de55||vt1240C.t5f2a_de55||vt1240C.t82_de55  ||vt1240C.t9f1a_de55||
                              vt1240C.t9f03_de55;

         -- JMG: valida si P55 es nulo
         IF vt1240C.De55 IS NULL THEN
            mb(55):='0';
            pqmonproc.inslog(vidproc, 'W', 'No Existe P55 para P31='||vt1240C.de_31);
         ELSE
            vt1240C.londat_p55 := lpad(length(vt1240C.De55)/2,3,'0');
         END IF;
--         vt1240C.londat_p55 := lpad(std.pf_dec2hex(length(vt1240C.De55)/2),3,'0');

      end if;


      mb(49):='1';     vt1240C.de_49 := c.montra_p49_actc;

      if nvl(c.CYCLID_P63_ACTC,' ') <> ' ' then
        mb(63):='1';     vt1240C.de_63 := '016 ' || rpad(substr(c.CYCLID_P63_ACTC,2,15),15,' ');
      end if;

      mb(71):='1';     vNumSecReg    := vNumSecReg + 1;
                       vt1240C.de_71 := lpad(vNumSecReg,8,0);

      mb(94):='1';     vt1240C.de_94 := vt1240C.de_33;
--      mb(95):='1';     vt1240C.de_95 := '10'||substr(c.numemi_p95_actc,1,10);

       if c.codfun_p24_actc != '200'  then
         mb(95):='1';   vt1240C.de_95 := '10'||vt1240C.TransationID;
      end if;



      -- Concatenar los campos del registro en linea
      strHexa1240C:= rtrim(ltrim(f_gen_MapaHex2(mb)));
      vt1240C.bitpri_actc := substr(strHexa1240C,1,16);
      vt1240C.bitsec_actc := substr(strHexa1240C,17,16);

      vcad1240:= vt1240C.idemen_p00_actc || vt1240C.bitpri_actc      || vt1240C.bitsec_actc      ||
                 vt1240C.lontar_l02_actc || vt1240C.numtar_p02_actc  || vt1240C.codpro_p03_actc  ||
                 vt1240C.imptra_p04_actc || vt1240C.timloc_p12_actc  || vt1240C.feccad_p14_actc  ||
                 vt1240C.punser_p22_actc || vt1240C.numsec_p23_actc  || vt1240C.de_24            || vt1240C.de_25  ||
                 vt1240C.de_26           || vt1240C.de_30            || vt1240C.de_31  ||
                 vt1240C.de_32           ||
                 vt1240C.de_33           || vt1240C.de_37            || vt1240C.de_38  || vt1240C.de_40 ||
                 vt1240C.de_41           || vt1240C.de_42            || vt1240C.de_43  ||
                 vt1240C.londat_P48      || vt1240C.De48             || vt1240C.de_49  ||
                 vt1240C.moncon_p50_actc || vt1240C.londat_p55       || vt1240C.De55   ||
                 vt1240C.de_63           || vt1240C.de_71            ||
                 vt1240C.londat_de72     || vt1240C.de_72            || vt1240C.de_94  ||
                 vt1240C.de_95;
      --formato fijo a 1024 posiciones
      --vlinea   := rpad(vlinea,1024,' ');

      utl_file.put_raw(vidfile, utl_raw.cast_to_raw(vcad1240),true);
      utl_file.new_line(vidfile);
      utl_file.fflush(vidfile);

      -- Inicio: Calculos para Registro de Conciliacion (1240)
      vMtoTot_1240.TotalMensaje  := vMtoTot_1240.TotalMensaje + 1;
end loop;

end;







procedure p_GenRegAnulacion(pfecha  varchar2,pbanco varchar2,pcodhcierre CHAR) is
    strhexa1240C       varchar2(32)     := '';
    vcodpostal         varchar2(10)     := '';
    vcodestado         varchar2(3)      := '';
    long_p48           number           := 0;
    vbanco             varchar2(2)      := '';
    vt1240C            msg_1240C;
    mb                 Mapa_Bits;
    vs                 s;
    vIrd               varchar2(2)      := '';
    pds0158_de48       VARCHAR2(20)    :='';
    mb40               VARCHAR2(1)     :='';
    vcodser            VARCHAR2(3)     :=NULL;
/*    vlong              varchar2(128);
    vdato              varchar2(1024);
    ncero              number := 0;

    videmen_p00_actc   clr_dcemvfull.idemen_p00_actc%TYPE;
    videtra_p11_actc   clr_dcemvfull.idetra_p11_actc%TYPE;
    vtimloc_p12_actc   clr_dcemvfull.timloc_p12_actc%TYPE;
    videadq_p32_actc   clr_dcemvfull.ideadq_p32_actc%TYPE;
    vemvtvr_p55_actc   clr_dcemvfull.emvtvr_p55_actc%TYPE;
    vperint_p55_actc   clr_dcemvfull.perint_p55_actc%TYPE;
    vnumale_p55_actc   clr_dcemvfull.numale_p55_actc%TYPE;
    vcapter_p55_actc   clr_dcemvfull.capter_p55_actc%TYPE;
    vscremi_p55_actc   clr_dcemvfull.scremi_p55_actc%TYPE;
    vnumsec_p23_actc   clr_dcemvfull.numsec_p23_actc%TYPE;
    vdaplem_p55_actc   clr_dcemvfull.daplem_p55_actc%TYPE;
    vttcrip_p55_actc   clr_dcemvfull.ttcrip_p55_actc%TYPE;
    vemvatc_p55_actc   clr_dcemvfull.emvatc_p55_actc%TYPE;
    vcrippe_p55_actc   clr_dcemvfull.crippe_p55_actc%TYPE;
    vpaicri_p55_actc   clr_dcemvfull.paicri_p55_actc%TYPE;
    vfeccri_p55_actc   clr_dcemvfull.feccri_p55_actc%TYPE;
    vimpcri_p55_actc   clr_dcemvfull.impcri_p55_actc%TYPE;
    vinfcri_p55_actc   clr_dcemvfull.infcri_p55_actc%TYPE;
    vmoncri_p55_actc   clr_dcemvfull.moncri_p55_actc%TYPE;
    vimccri_p55_actc   clr_dcemvfull.imccri_p55_actc%TYPE;*/


cursor cr_Anulacion is
    select idemen_p00_actc,   lontar_l02_actc,   numtar_p02_actc,
           codpro_p03_actc,   imptra_p04_actc,   idetra_p11_actc, timloc_p12_actc,
           feccad_p14_actc,   punser_p22_actc,   codfun_p24_actc,
           codraz_p25_actc,   codact_p26_actc,   lpad(nvl(impori_p30_actc,0),12,0) impori_p30_actc,
           lonref_l31_actc,   refadq_p31_actc,   ideadq_l32_actc,
           ideadq_p32_actc,   idepre_l33_actc,   idepre_p33_actc, pista2_p35_actc,
           datref_p37_actc,   numaut_p38_actc,   ideter_p41_actc,
           ideest_p42_actc,   nomest_p43_actc,    locest_p43_actc,
           decode(paiest_p43_actc,'VE','VEN','VEN')paiest_p43_actc,
           pds0025_p48,
           londat_p48_actc,   montra_p49_actc,   moncon_p50_actc,CYCLID_P63_ACTC,
           nummen_p71_actc,   numemi_l95_actc,   numemi_p95_actc,
           sesion_p28_actc,   oriide_p56_actc,   codser_p40_actc
     from rechazos_retornos
    where trunc(fec_upd)       = to_date(pfecha,'yyyymmdd') --> crosadof -> fix bug de la fecha....
    and   SUBSTR(ideadq_p32_actc,3,4)  = pbanco
    and   idemen_p00_actc  = '1240'
    and   estado           = 'T'
    and   tipo_incoming    = 'NA'  -- Tipo de incoming Naiguata 'NA' IPR 1302
    --and   codpro_p03_actc  = '000000'
    --and   oriide_p56_actc  = '1240'; -- se agrego para q solo se carguen las anulaciones contables
    --and   codfun_p24_actc  = '205'
    and   upper(substr(pds0025_p48,1,1))  = 'R';
--    and   substr(tiptra_p48_actc,1,2)     = '06';


 begin

   --inicia Generacion de Compra
   vPaso   := 'Paso 05';
   if pbanco = '0105' then vbanco :='BM'; end if;
   if pbanco = '0108' then vbanco :='BP'; end if;

   for c in cr_Anulacion  loop
      long_p48:=0;

      vt1240C     := f_ini_msg1240C;

      mb:= f_inimapabits;
      -- presencia del mapa de bits secundario
      mb(1):='1';

      -- identificador de tipo de mensaje ('1240')
      vt1240C.idemen_p00_actc :='1240';--to_char(c.idemen_p00_actc);

      mb(2) :='1';     vt1240C.lontar_l02_actc := to_char(c.lontar_l02_actc);
                       vt1240C.numtar_p02_actc := c.numtar_p02_actc;

      mb(3) :='1';     vt1240C.codpro_p03_actc := lpad(c.codpro_p03_actc,6,0);

      mb(4) :='1';     vt1240C.imptra_p04_actc := to_char(lpad(c.imptra_p04_actc,12,0));
                       vmontoTotal             := vmontoTotal + to_number(nvl(vt1240C.imptra_p04_actc,0));
                       vtotalMensaje           := vtotalMensaje + 1;

      mb(12):='1';     vt1240C.timloc_p12_actc := c.timloc_p12_actc;

      if c.feccad_p14_actc is not null then
         mb(14):='1';    vt1240C.feccad_p14_actc  := c.feccad_p14_actc;
      end if;

      mb(22):='1';
      if c.punser_p22_actc is null then
          vt1240C.punser_p22_actc := '000000000000';
      else
          vs.s1  := substr(c.punser_p22_actc,1,1);
          vs.s2  := substr(c.punser_p22_actc,2,1);
          vs.s3  := substr(c.punser_p22_actc,3,1);
          vs.s4  := substr(c.punser_p22_actc,4,1);
          IF  vs.s4 ='M' THEN -- realizado por Carlos Brito para: valida campo 22 subcampo 4, M es igual a mPOs y se modifica para que mande el valor 1 fecha 05/05/2017
              vs.s4:='1' ;
          END IF;
          vs.s6  := substr(c.punser_p22_actc,6,1);
          vs.s9  := substr(c.punser_p22_actc,9,1);
          vs.s10 := substr(c.punser_p22_actc,10,1);
          vs.s11 := substr(c.punser_p22_actc,11,1);
          vs.s12 := substr(c.punser_p22_actc,12,1);

          vs.s5 := substr(c.punser_p22_actc,5,1);
          if vs.s5 = '2' then    vs.s5 := '3';     end if;
          if vs.s5 = '3' then    vs.s5 := '4';     end if;
          if vs.s5 = '7' or      vs.s5 = '8'       then     vs.s5 := '5'; end if;

          vs.s7 := substr(c.punser_p22_actc,7,1);
          if vs.s7 = '5' then    vs.s7 := 'C';     end if;
          if vs.s7 = 'S' or      vs.s7  = 'T'      then     vs.s7 := 'B'; end if;
          if vs.s5 = '7' then    vs.s7 := 'T';     end if;
          if vs.s5 = '8' then    vs.s7 := 'S';     end if;

          vs.s8 := substr(c.punser_p22_actc,8,1);
          if vs.s8 = '8' then    vs.s8 := '6';     end if;
          --se concatena todas las cadenas
          vt1240C.punser_p22_actc := vs.s1||vs.s2||vs.s3||vs.s4||vs.s5||vs.s6||vs.s7||vs.s8||vs.s9||vs.s10||vs.s11||vs.s12;
       end if;

      mb(24):='1';   vt1240C.de_24   := c.codfun_p24_actc;
 /*******************************************
      mb(25):='1';   vt1240C.de_25   := lpad(c.codraz_p25_actc,4,0);
      ******************************************/
      if  c.codfun_p24_actc  = '200'  then
          if c.codraz_p25_actc = '1377' and c.impori_p30_actc = 0 then
             mb(25):='1';
             vt1240C.de_25 := '1401';
          else
             mb(25):='1';
             vt1240C.de_25 := '1402';
          end if;
      else
           if  c.codfun_p24_actc != '200'  then
               mb(25):='1';
               vt1240C.de_25   := c.codraz_p25_actc;
           end if;
      end if;

/**      if vt1240C.de_25  = '1377' and vt1240C.de_30 = 0 and vt1240C.imptra_p04_actc != 0 then
            mb(25):='1';
            vt1240C.de_25  := '1401';
      else
         if vt1240C.de_24  = '200' then
            if vt1240C.imptra_p04_actc != vt1240C.de_30 and vt1240C.de_30 != 0 then
               mb(25):='1';
               vt1240C.de_25  := '1402';
            end if;
         else
            mb(25):='1';
            vt1240C.de_25   := c.codraz_p25_actc;
          end if;
      end if;*/

/**      if vt1240C.de_25  = '1377' and vt1240C.de_30 = 0 and vt1240C.imptra_p04_actc != 0 and vt1240C.de_24  = '200'  then
            mb(25):='1';
            vt1240C.de_25  := '1401';
      else
         if vt1240C.de_24  = '200' then
            if vt1240C.imptra_p04_actc != vt1240C.de_30 and vt1240C.de_30 != 0 then
               mb(25):='1';
               vt1240C.de_25  := '1402';
            end if;
         else
            mb(25):='1';
            vt1240C.de_25   := c.codraz_p25_actc;
          end if;
      end if; **/

      mb(26):='1';   vt1240C.de_26   := lpad(c.codact_p26_actc,4,0);
      -- Solo para transacciones cuyo mcc = 7995(11/09/2009)
      if vt1240C.de_26 in ('4829','7995') then
         vt1240C.codpro_p03_actc := '180000';
      end if;

      mb(30):='1';
                     vt1240C.de_30s1  := to_char(lpad(c.imptra_p04_actc,12,0));
                     vt1240C.de_30s2  := '000000000000';
                     vt1240C.de_30    := vt1240C.de_30s1 || vt1240C.de_30s2;

      mb(31):='1';   vt1240C.de_31            :=  lpad(to_char(c.lonref_l31_actc),2,0)||lpad(c.refadq_p31_actc,23,0);

/*      mb(32):='1';
      if vbanco = 'BM' then  vt1240C.de_32 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_32 := '06010403'; end if;

      mb(33):='1';   vt1240C.de_33            :=  lpad(to_char(c.idepre_l33_actc),2,0)||c.idepre_p33_actc;*/
      mb(32):='1';
      if vbanco = 'BM' then  vt1240C.de_32 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_32 := '06010403'; end if;

--      mb(33):='1';   vt1240C.de_33            :=  lpad(to_char(c.idepre_l33_actc),2,0)||c.idepre_p33_actc;
      mb(33):='1';
      if vbanco = 'BM' then  vt1240C.de_33 := '06010375'; end if;
      if vbanco = 'BP' then  vt1240C.de_33 := '06010403'; end if;

      mb(37):='1';   vt1240C.de_37            :=  rpad(c.datref_p37_actc,12,0);
      mb(38):='1';   vt1240C.de_38            :=  rpad(c.numaut_p38_actc,6,0);
      mb(41):='1';   vt1240C.de_41            :=  rpad(c.ideter_p41_actc,8,0);
      mb(42):='1';   vt1240C.de_42            :=  rpad(c.ideest_p42_actc,15,' ');
      mb(43):='1';   vt1240C.nomest_p43       :=  rpad(substr(c.nomest_p43_actc,1,22),22,' ')||'\';
                     vt1240C.dircomercio_p43  :=  '\';
                     vt1240C.locest_p43       :=  rpad(c.locest_p43_actc,13,' ')||'\';

                     p_findPostalCode(c.ideest_p42_actc,vcodpostal,vcodestado);
                     vcodpostal := pqutlcom.p_PostalCodexState(vcodestado); -- IPR 1080 CODIGO POSTAL POR ESTADO
                     vt1240C.cod_postal_p43   := rpad(vcodpostal,10,' ');
                     vt1240C.cod_estado_p43   := rpad(vcodestado,3,' ');
                     vt1240C.paiest_p43       := rpad(c.paiest_p43_actc,3,' ');
                     vt1240C.longdat_p43      := lpad(length(vt1240C.nomest_p43||
                                                        vt1240C.dircomercio_p43||
                                                        vt1240C.locest_p43||
                                                        vt1240C.cod_postal_p43||
                                                        vt1240C.cod_estado_p43||
                                                        vt1240C.paiest_p43),2,0);

                    vt1240C.de_43  :=  vt1240C.longdat_p43     || vt1240C.nomest_p43     ||
                                       vt1240C.dircomercio_p43 || vt1240C.locest_p43     ||
                                       vt1240C.cod_postal_p43  || vt1240C.cod_estado_p43 ||
                                       vt1240C.paiest_p43;

      mb(48):='1';
      --de48_pds0025
      if upper(substr(c.pds0025_p48,1,1)) = 'R' then
         vt1240C.pds0025_de48 := '0025007R'||substr(c.pds0025_p48,2,6);
      end if;
      --de48_pds0148
      --vt1240C.isnum_nacional := f_FindTarjetaNacional(c.numtar_p02_actc,vbanco);
      --if vt1240C.isnum_nacional > 0 then
         vt1240C.pds0148_de48   := '0023003NA 0148004'||c.montra_p49_actc||'2';
      --else
      --   vt1240C.pds0148_de48   := '0023003NA 01480048402';
      --end if;
      --de48_pds0149
      vt1240C.pds0149_de48   := '0149006'||c.montra_p49_actc||'000';
      --de48_pds0158
      if c.codfun_p24_actc != '200'  then
         p_findTransactionIdDisputas(c.refadq_p31_actc, c.numtar_p02_actc,vt1240C.TransationID,vt1240C.SourceAmount,vt1240C.currenCode,vt1240C.pds0262_Ide48);
         vt1240C.pds0262_de48   := '0262001'||vt1240C.pds0262_Ide48;
      end if;

      p_findird(vt1240C.de_26, vt1240C.cod_mcc, vt1240C.cod_ird, vt1240C.cod_tcc );

      --if vt1240C.isnum_nacional > 0 then     --Tarjetas Nacionales

         if  to_number(vt1240C.de_26)  = to_number(vt1240C.cod_mcc)  then
             vt1240C.pds0158_de48 := '0158012MCC       '||lpad(to_char(vt1240C.cod_ird),2,'0');
         end if;

      /*else                                    --Tarjetas Corporativas

         --p_getDataInternacional(c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region);
         pqoutgoingmc.p_getDataInternacional(vIDProc,c.numtar_p02_actc,vbanco, vt1240C.Cod_Producto_GCMS, vt1240C.cod_licen_producto, vt1240C.cod_pais, vt1240C.cod_region,vt1240C.cod_programa);
         vt1240C.isBetweenFiveday := f_isBetweenFiveday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));
         vt1240C.isBetweenThirtyday := f_isBetweenThirtyday(to_date(substr(c.timloc_p12_actc,1,6),'yymmdd'));

         vcodser := LPAD(NVL(c.codser_p40_actc,'0'),3,'0');

         vt1240C.FecPresentacion := SUBSTR(c.TIMLOC_P12_ACTC,1,6);

         if vt1240C.Cod_Producto_GCMS in ('DAG','DAP','DAS','DOS','SAG','SAP','SAS','SOS','WBE','MBP','MBT','MTP','MDJ','MRH') then
            vt1240C.pds0002_de48 := vt1240C.Cod_Producto_GCMS ;
            vt1240C.pds0003_de48 := vt1240C.cod_licen_producto;
         else
            vt1240C.pds0002_de48 := '';
            vt1240C.pds0003_de48 := '';
         end if;

         pqoutgoingmc.p_Obtiene_IRD(vt1240C.Cod_Producto_GCMS,vt1240C.cod_region,c.punser_p22_actc,vt1240C.punser_p22_actc,vcodser,vt1240C.isBetweenFiveday,vt1240C.isBetweenThirtyday,vt1240C.FecPresentacion,vt1240C.cod_programa,pds0158_de48,mb40);

         vt1240C.pds0158_de48 := pds0158_de48;

         IF mb40 = '1' THEN
            mb(40):= mb40;
            vt1240C.de_40 := vcodser;
         END IF;

      END IF;*/

      vt1240C.pds0165_de48 := '0165001M';

      vt1240C.De48         := vt1240C.pds0025_de48||  vt1240C.pds0148_de48||
                              vt1240C.pds0149_de48||  vt1240C.pds0158_de48||
                              vt1240C.pds0165_de48||  vt1240C.pds0262_de48;

      vt1240C.londat_P48 := Lpad(length(vt1240C.De48),3,'0');

/*      if SUBSTR(c.punser_p22_actc,7,1) = '5' then

         select emvtvr_p55_actc, perint_p55_actc, numale_p55_actc, capter_p55_actc,
                scremi_p55_actc, numsec_p23_actc, daplem_p55_actc, ttcrip_p55_actc,
                infcri_p55_actc, emvatc_p55_actc, crippe_p55_actc, paicri_p55_actc,
                feccri_p55_actc, impcri_p55_actc, moncri_p55_actc, imccri_p55_actc
           into vemvtvr_p55_actc, vperint_p55_actc, vnumale_p55_actc, vcapter_p55_actc,
                vscremi_p55_actc, vnumsec_p23_actc, vdaplem_p55_actc, vttcrip_p55_actc,
                vinfcri_p55_actc, vemvatc_p55_actc, vcrippe_p55_actc, vpaicri_p55_actc,
                vfeccri_p55_actc, vimpcri_p55_actc, vmoncri_p55_actc, vimccri_p55_actc
           from CLR_DCEMVFULL
          where idemen_p00_actc = c.idemen_p00_actc
            and idetra_p11_actc = c.idetra_p11_actc
            and timloc_p12_actc = c.timloc_p12_actc
            and ideadq_p32_actc = c.ideadq_p32_actc;

         mb(23):='1';
         vt1240C.numsec_p23_actc := lpad(vnumsec_p23_actc,3,'0');

         mb(55):='1';
         if vcrippe_p55_actc is null then
            vt1240C.t9f26_de55 := '';
         else
            vdato := std.PF_BCD2HEX(vcrippe_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vcrippe_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
--            ncero := length(vlong);
--            if ncero not in (2,4,6,8,10) then
--               vt1240C.t9f26_de55 := '9F26'||lpad(vlong,ncero+1,'0')||vdato;
--            else
--               vt1240C.t9f26_de55 := '9F26'||vlong||vdato; --std.pf_hex2bin('9F26')||vlong||vdato;
--            end if;
            vt1240C.t9f26_de55 := '9F26'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
--            ncero := 0;
         end if;

         if vinfcri_p55_actc is null then
            vt1240C.t9f27_de55 := '';
         else
            vdato := std.PF_BCD2HEX(vinfcri_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vinfcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f27_de55 := '9F27'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F27')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vdaplem_p55_actc is null then
            vt1240C.t9f10_de55 := '';
         else
            vdato := std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)); --std.pf_hex2bin(std.PF_BCD2HEX(SUBSTR(vdaplem_p55_actc,1,31)));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f10_de55 := '9F10'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F10')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vnumale_p55_actc is null then
            vt1240C.t9f37_de55 := '';
         else
            vdato := std.PF_BCD2HEX(vnumale_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vnumale_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f37_de55 := '9F37'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F37')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vemvatc_p55_actc is null then
            vt1240C.t9f36_de55 := '';
         else
            vdato := std.PF_BCD2HEX(vemvatc_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvatc_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f36_de55 := '9F36'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F36')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vemvtvr_p55_actc is null then
            vt1240C.t95_de55 := '';
         else
            vdato := std.PF_BCD2HEX(vemvtvr_p55_actc); --std.pf_hex2bin(std.PF_BCD2HEX(vemvtvr_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t95_de55 := '95'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('95')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vfeccri_p55_actc is null then
            vt1240C.t9a_de55 := '';
         else
            vdato := std.pf_bcd2hex(vfeccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vfeccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9a_de55 := '9A'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9A')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vttcrip_p55_actc is null then
            vt1240C.t9c_de55 := '';
         else
            vdato := std.pf_bcd2hex(vttcrip_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vttcrip_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9c_de55 := '9C'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9C')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vimpcri_p55_actc is null then
            vt1240C.t9f02_de55 := '';
         else
            vdato := std.pf_bcd2hex(vimpcri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimpcri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f02_de55 := '9F02'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F02')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vmoncri_p55_actc is null then
            vt1240C.t5f2a_de55 := '';
         else
            vdato := std.pf_bcd2hex(vmoncri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vmoncri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t5f2a_de55 := '5F2A'||lpad(vlong,2,'0')||vdato; ----std.pf_hex2bin('5F2A')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vperint_p55_actc is null then
            vt1240C.t82_de55 := '';
         else
            vdato := std.pf_bcd2hex(vperint_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vperint_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8))
            vt1240C.t82_de55 := '82'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
         end if;

         if vpaicri_p55_actc is null then
            vt1240C.t9f1a_de55 := '';
         else
            vdato := std.pf_bcd2hex(vpaicri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vpaicri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f1a_de55 := '9F1A'||lpad(vlong,2,'0')||vdato;
            vlong := '';
            vdato := '';
         end if;

        if vimccri_p55_actc is null then
            vt1240C.t9f03_de55 := '';
         else
            vdato := std.pf_bcd2hex(vimccri_p55_actc); --std.pf_hex2bin(std.pf_bcd2hex(vimccri_p55_actc));
            vlong := std.pf_dec2hex(length(vdato)/2); --std.pf_hex2bin(std.pf_dec2hex(length(vdato) / 8));
            vt1240C.t9f03_de55 := '9F03'||lpad(vlong,2,'0')||vdato; --std.pf_hex2bin('9F03')||vlong||vdato;
            vlong := '';
            vdato := '';
         end if;

         vt1240C.De55      := vt1240C.t9f26_de55||vt1240C.t9f27_de55||vt1240C.t9f10_de55||vt1240C.t9f37_de55||
                              vt1240C.t9f36_de55||vt1240C.t95_de55  ||vt1240C.t9a_de55  ||vt1240C.t9c_de55  ||
                              vt1240C.t9f02_de55||vt1240C.t5f2a_de55||vt1240C.t82_de55  ||vt1240C.t9f1a_de55||
                              vt1240C.t9f03_de55;

--         vt1240C.londat_p55 := lpad(length(vt1240C.De55)/2,3,'0');
         vt1240C.londat_p55 := lpad(std.pf_dec2hex(length(vt1240C.De55)/2),3,'0');

      end if;*/

      mb(49):='1';     vt1240C.de_49 := c.montra_p49_actc;

      if nvl(c.CYCLID_P63_ACTC,' ') <> ' ' then
        mb(63):='1';     vt1240C.de_63 := '016 ' || rpad(substr(c.CYCLID_P63_ACTC,2,15),15,' ');
      end if;

      mb(71):='1';     vNumSecReg    := vNumSecReg + 1;
                       vt1240C.de_71 := lpad(vNumSecReg,8,0);

      mb(94):='1';     vt1240C.de_94 := vt1240C.de_33;
--      mb(95):='1';     vt1240C.de_95 := '10'||substr(c.numemi_p95_actc,1,10);

       if c.codfun_p24_actc != '200'  then
         mb(95):='1';   vt1240C.de_95 := '10'||vt1240C.TransationID;
      end if;




      -- Concatenar los campos del registro en linea
      strHexa1240C:= rtrim(ltrim(f_gen_MapaHex2(mb)));
      vt1240C.bitpri_actc := substr(strHexa1240C,1,16);
      vt1240C.bitsec_actc := substr(strHexa1240C,17,16);

      vcad1240:= vt1240C.idemen_p00_actc || vt1240C.bitpri_actc      || vt1240C.bitsec_actc      ||
                 vt1240C.lontar_l02_actc || vt1240C.numtar_p02_actc  || vt1240C.codpro_p03_actc  ||
                 vt1240C.imptra_p04_actc || vt1240C.timloc_p12_actc  || vt1240C.feccad_p14_actc  ||
                 vt1240C.punser_p22_actc || vt1240C.numsec_p23_actc  || vt1240C.de_24            || vt1240C.de_25  ||
                 vt1240C.de_26           || vt1240C.de_30            || vt1240C.de_31  ||
                 vt1240C.de_32           ||
                 vt1240C.de_33           || vt1240C.de_37            || vt1240C.de_38  || vt1240C.de_40 ||
                 vt1240C.de_41           || vt1240C.de_42            || vt1240C.de_43  ||
                 vt1240C.londat_P48      || vt1240C.De48             || vt1240C.de_49  ||
                 vt1240C.moncon_p50_actc || vt1240C.londat_p55       || vt1240C.De55   ||
                 vt1240C.de_63           || vt1240C.de_71            ||
                 vt1240C.londat_de72     || vt1240C.de_72            || vt1240C.de_94  ||
                 vt1240C.de_95;
      --formato fijo a 1024 posiciones
      --vlinea   := rpad(vlinea,1024,' ');

      utl_file.put_raw(vidfile, utl_raw.cast_to_raw(vcad1240),true);
      utl_file.new_line(vidfile);
      utl_file.fflush(vidfile);

      -- Inicio: Calculos para Registro de Conciliacion (1240)
      vMtoTot_1240.TotalMensaje  := vMtoTot_1240.TotalMensaje + 1;
end loop;

end;








procedure p_GenReg1644(vcodfuncion number,vfecha varchar2, vbanco varchar2)as
    m1644          Msg_1644;
    vbit_pri       varchar2(16)     := '';
    vbit_sec       varchar2(16)     := '';
    bank           varchar2(11)     := '';
    vbancoadq      varchar2(6)      := '';
    vbanco1744     varchar2(6)      := '';

Begin
      --inicia proceso de outgoing Banco Provincial
      vPaso   := 'Paso 06';

      m1644.idemen_p00_actc   := '1644';
      vbit_pri                := '8000010000010000';
      vbit_sec                := '0200000000000000' ;


      if vbanco = 'BM'  then
             bank        := lpad('010375',11,0);
             vbancoadq   := '540105';
             vbanco1744  := '990105';
         else if vbanco = 'BP' then
             bank        := lpad('010403',11,0);
             vbancoadq   := '540108';
             vbanco1744  := '990108';
         end if;
      end if;


      vNumSecReg             := vNumSecReg + 1;
      m1644.nummen_p71_actc  := to_char(lpad((vNumSecReg),8,0));


      --cabecera de registro
      if vcodfuncion = 697 then
           m1644.pds0105_p48      := '0105025002'||vfecha||bank||lpad(to_char(vSecControl),5,0);
           m1644.pds0122_p48      := '0122001'||vTipFileMC;

           m1644.londat_p48_actc  := lpad(length(m1644.pds0105_p48||
                                                 m1644.pds0122_p48),3,0);

           m1644.de_p48           :=  m1644.pds0105_p48||   m1644.pds0122_p48||
                                      m1644.pds0301_p48||   m1644.pds0306_p48;
      end if;

      --trailer de registro
       if  vcodfuncion = 695 then
          m1644.pds0105_p48     := '0105025002'||vfecha||bank||lpad(to_char(vSecControl),5,0);
          --m1644.pds0122_p48     := '0122001T';
          m1644.pds0301_p48     := '0301016'||lpad(to_char(nvl(vmontoTotal,0)),16,0);
          m1644.pds0306_p48     := '0306008'||to_char(lpad(nvl(vtotalMensaje,0)+ 2,8,0));

          m1644.londat_p48_actc := lpad(length(m1644.pds0105_p48||
                                               m1644.pds0301_p48||
                                               m1644.pds0306_p48),3,0);

          m1644.de_p48          := m1644.pds0105_p48|| m1644.pds0301_p48||m1644.pds0306_p48;
       end if;

     m1644.codfun_p24_actc      :=   vcodfuncion;

     vcad1240:= m1644.idemen_p00_actc ||   vbit_pri||               vbit_sec||
                m1644.codfun_p24_actc ||   m1644.codraz_p25_actc||  m1644.codact_p26_actc||
                m1644.impori_p30_actc ||   m1644.refadq_p31_actc||  m1644.londat_p48_actc||
                m1644.de_p48||             m1644.nummen_p71_actc;

     --formato fijo a 1024 posiciones
     --vlinea   := rpad(vlinea,1024,' ');

     utl_file.put_raw(vidfile, utl_raw.cast_to_raw(vcad1240),true);
     utl_file.new_line(vidfile);
     utl_file.fflush(vidfile);

End;









procedure p_GenReg1740(pfecha  varchar2, pbanco varchar2) is
    strhexa1240C       varchar2(32)     := '';
    vcodpostal         varchar2(10)     := '';
    vcodestado         varchar2(3)      := '';
    vcad1740           varchar2(999)    := '';
    long_p48           number           := 0;
    vbanco             varchar2(2)      := '';
    idepre             varchar2(6)      := '';
    vfecJuliana        varchar2(4)      := '';
    vbinAdq            varchar2(6)      := '';
    vt1740             msg_1740;
    mb                 Mapa_Bits;
    vs                 s;


cursor cr_1740 is
    select idemen_p00_actc,   lontar_l02_actc,   numtar_p02_actc,
           codpro_p03_actc,   imptra_p04_actc,   idetra_p11_actc,
           timloc_p12_actc,
           feccad_p14_actc,   punser_p22_actc,   codfun_p24_actc,
           codraz_p25_actc,   codact_p26_actc,   lpad(nvl(impori_p30_actc,0),12,0) impori_p30_actc,
           lonref_l31_actc,   refadq_p31_actc,   ideadq_l32_actc,
           ideadq_p32_actc,   idepre_l33_actc,   idepre_p33_actc,
           datref_p37_actc,   numaut_p38_actc,   ideter_p41_actc,
           ideest_p42_actc,   nomest_p43_actc,    locest_p43_actc,
           decode(paiest_p43_actc,'VE','VEN','VEN')paiest_p43_actc,
           impcu1_p46_actc,   sigcu1_p46_actc,     pds0025_p48,
           londat_p48_actc,   montra_p49_actc,   moncon_p50_actc,
           nummen_p71_actc,   numemi_l95_actc,   numemi_p95_actc,
           sesion_p28_actc
     from rechazos_retornos
    where trunc(fec_upd)          = to_date(pfecha,'yyyymmdd')
    --and   SUBSTR(ideadq_p32_actc,3,4)  = pbanco --no se envia el P32
    and   idepre_p33_actc  = idepre
    and   idemen_p00_actc  = '1740'
    and   estado           = 'T'
    and   tipo_incoming    = 'MC';
    --and   codfun_p24_actc  = '205';

begin
   --inicia Generacion mensajes 1740
   vPaso   := 'Paso 07';

   if pbanco = '0105' then vbanco :='BM'; end if;
   if pbanco = '0108' then vbanco :='BP'; end if;

      if vbanco = 'BM' then  idepre := '010375'; end if;
      if vbanco = 'BP' then  idepre := '010403'; end if;

   for c in cr_1740  loop
      vcad1740  := '';
      long_p48  := 0;
      vt1740    := f_ini_msg1740;

      mb:= f_inimapabits;
      -- presencia del mapa de bits secundario
      mb(1) :='1';   vt1740.mti    := '1740';
      mb(2) :='1';   vt1740.de02   := lpad(to_char(c.lontar_l02_actc),2,0)||c.numtar_p02_actc;
      --mb(3) :='1';   vt1740.de03   := '190000';

      if c.sigcu1_p46_actc = 'C' then
         mb(3) :='1';   vt1740.de03   := '290000';
      elsif c.sigcu1_p46_actc = 'D' then
         mb(3) :='1';   vt1740.de03   := '190000';
      end if;

      mb(4) :='1';   vt1740.de04   := lpad(to_char(c.impcu1_p46_actc),12,0);
                     vmontoTotal   := vmontoTotal + to_number(nvl(vt1740.de04,0));
                     vtotalMensaje := vtotalMensaje + 1;

      --Modificado GLOBALR 16-03-2017 IPR 1137 - Inclusi¿n del capo DE14 a la estructura
      IF c.feccad_p14_actc IS NOT NULL THEN
         mb(14):='1';
         vt1740.de14  := c.feccad_p14_actc;
      END IF;
      --/Fin de la modificaci¿n GLOBALR

      mb(24):='1';   vt1740.de24   := c.codfun_p24_actc;
      mb(25):='1';   vt1740.de25   := f_FindCodigoRazon(c.codraz_p25_actc,c.numtar_p02_actc);

 /*     mb(33):='1';
          if vbanco  = 'BM' then  vt1740.de33  := '06010375'; end if;
          if vbanco  = 'BP' then  vt1740.de33  := '06010403'; end if;*/

      mb(33):='1';
      if vbanco = 'BM' then  vt1740.de33 := '06010375'; end if;
      if vbanco = 'BP' then  vt1740.de33 := '06010403'; end if;

      mb(48):='1';
                      --de48_pds0025
                      if upper(substr(c.pds0025_p48,1,1)) = 'R' then
                         vt1740.de48_pds0025 := '0025007R'||substr(c.pds0025_p48,2,6);
                      end if;

                      --de48_pds0137
                      if vbanco = 'BM' then   vbinadq := '550291'; end if;
                      if vbanco = 'BP' then   vbinadq := '550291'; end if;

                      select to_char(sysdate,'y')||to_char(sysdate,'ddd')
                      into vfecJuliana
                      from dual;

                      vt1740.pds0137_cad   := '7'||vbinadq||vfecJuliana||c.idetra_p11_actc;
                      vt1740.digCheck      := f_GenCheckDigit(vt1740.pds0137_cad);
                      vt1740.pds0137_lon   := lpad(length(vt1740.pds0137_cad||to_char(vt1740.digCheck)),3,0);
                      vt1740.de48_pds0137  := '0137'||vt1740.pds0137_lon||vt1740.pds0137_cad||to_char(vt1740.digCheck);

                      --de48_pds0148
                      vt1740.isnum_nacional := f_FindTarjetaNacional(c.numtar_p02_actc,vbanco);
                      vt1740.de48_pds0148   := '0148004'||vmoney||'2';

                      vt1740.de48_pds0165   := '0165001M';

                      vt1740.De48           := vt1740.de48_pds0025||    vt1740.de48_pds0137||
                                               vt1740.de48_pds0148||    vt1740.de48_pds0165;

                      vt1740.de48_londat := Lpad(length(vt1740.De48),3,'0');

      mb(49):='1';    vt1740.de49   := vMoney;
      mb(71):='1';    vNumSecReg    := vNumSecReg + 1;
                      vt1740.de71   := lpad(vNumSecReg,8,0);

      mb(73):='1';    vt1740.de73   := substr(c.timloc_p12_actc,1,6);


      mb(93):='1';
      if c.numtar_p02_actc is null then
           vt1740.de93   := '00003001';
        else
           vt1740.de93   := '11'||lpad(f_getIdBankDestino(c.numtar_p02_actc,vbanco),11,0);
      end if;


      mb(94):='1';    vt1740.de94   := vt1740.de33;

      strHexa1240C:= rtrim(ltrim(f_gen_MapaHex2(mb)));
      vt1740.bitpri := substr(strHexa1240C,1,16);
      vt1740.bitsec := substr(strHexa1240C,17,16);

-- MOdificado GLOBALR 16-03-2017 IPR 1137 Inclusi¿n de vt1740.de14
      vcad1740  :=  vt1740.mti             || vt1740.bitpri         || vt1740.bitsec
                 || vt1740.de02            || vt1740.de03           || vt1740.de04
                 || vt1740.de14            || vt1740.de24            || vt1740.de25
                 || vt1740.de33            || vt1740.de48_londat     || vt1740.De48
                 || vt1740.de49            || vt1740.de71            || vt1740.de73
                 || vt1740.de93            || vt1740.de94;
-- MOdificado GLOBALR FIN
      utl_file.put_line(vidfile,vcad1740,true);

      vMtoTot_1240.TotalMensaje  := vMtoTot_1240.TotalMensaje + 1;
end loop;
end;







procedure p_FindPostalCode(pcodComercio char, pcodePostal out varchar2, pCodEstado out number)is

 begin

   select codigo_postal,cod_estado
     into pcodePostal,pcodEstado
     from comercios_pmp t
    where t.cod_comercio = trim(PcodComercio);

 exception
    when no_data_found  then
      verrmsg := 'No existe Codigo Postal y de Estado para el Comercio '||pcodComercio;
      verrcod := '9';
      raise efinerror;

   when too_many_rows then
      verrmsg := 'Hay mas de un Codigo Postal y de Estado para el Comercio '||pcodComercio;
      verrcod := '10';
      raise efinerror;
end;








function f_IniMapaBits return mapa_bits is
    num1 number:=0;
    vmapabits  mapa_bits;
begin
    vmapabits:=mapa_bits();
    for num1 in 1 ..128 loop
        vmapabits.extend;
        vmapabits(num1):='0';
    end loop;
    return vmapabits;
end;







function f_IniArrayLuhn return ArrayLuhn is
    i number:=0;
    vArrayLuhn  ArrayLuhn;
begin
    vArrayLuhn:=ArrayLuhn();
    for i in 1 ..17 loop
        vArrayLuhn.extend;
        vArrayLuhn(i):='';
    end loop;
    return vArrayLuhn;
end;





function f_Bin_Hex(bin varchar2) return char
-- InPut : cadena binaria de 4 caracteres
-- OutPut: cadena hexadecimal
is
  i        number;
  n        number;
  valor    number:=0;
  vTotal   number:=0;

  type t_binaria is varray(4) of number;
  type t_hexadecimal is varray(16) of char;
  v_binario t_binaria := t_binaria(8,4,2,1);
  v_hex t_hexadecimal := t_hexadecimal('A','B','C','D','E','F');

begin

    for i in 1 .. 4 loop
        valor:=0;
        n:= to_number(substr(bin,i,1));
        if n=1 then
           valor := v_binario(i);
        end if;
        vTotal:= vTotal + valor;
    end loop;

    if vTotal> 9 then
        for i in 10..15 loop
            if i=vTotal then
               return to_char(v_hex(i-9));
            end if;
        end loop;
    else
        return to_char(vTotal);
    end if;
end ;






function f_Gen_MapaHex2(matrizBin in out Mapa_Bits) return varchar
-- InPut  : Matriz binaria de 128 bits
-- OutPut : Cadena Hexadecimal de 32 bits
is
i number:=0;
Cad_Bin4 varchar2(4):='';
v_Hex char(1):='';
Cad_Hex varchar2(32):='';
begin
    Cad_Bin4:='';
    Cad_Hex:='';
    for i in 1..128 loop
        Cad_Bin4:=Cad_Bin4 || matrizBin(i);
        if length(Cad_Bin4) = 4 then
            v_Hex:= f_bin_hex(Cad_Bin4);
            Cad_Hex := Cad_Hex || v_Hex;
            Cad_Bin4:='';
        end if;
    end loop;
    return Cad_Hex;
end;







function f_ini_msg1240C return Msg_1240C is
    vtc_Msg_1240C  Msg_1240C;
  begin
    return vtc_Msg_1240C;
end ;








function f_ini_msg1740 return Msg_1740 is
    vtc_Msg_1740  Msg_1740;
  begin
    return vtc_Msg_1740;
end ;







function f_iniCardAceptorBusiness return vb_CardAceptorBusiness is
    t_vb_CardAceptorBusiness  vb_CardAceptorBusiness;
  begin
    return t_vb_CardAceptorBusiness;
end ;







function f_findTipoCambio(pbanco varchar2,psfecha varchar2)return pls_integer is
     vtip_cambio     pls_integer;
     vsfecha         varchar2(10);
     vdate           date;

begin
    vdate    := to_date(psfecha,'yyyy/mm/dd');
    vsfecha  := to_char(vdate,'dd/mm/yyyy');

    select tipo_cambio
      into vtip_cambio
      from sgdvnz.disputas_tipo_cambio_incoming
     where cod_entadq    = pbanco
       and cod_marca     = 'MC'
       and incoming_date = to_date(psfecha,'yyyymmdd');

     return vtip_cambio;

exception
   when no_data_found  then
      verrmsg := 'No existe tipo de cambio para el dia '||vsfecha;
      verrcod := '7';
      raise efinerror;

   when too_many_rows then
      verrmsg := 'No puede haber mas un tipo de cambio para un mismo dia';
      verrcod := '8';
      raise efinerror;
end;






function f_FindTarjetaNacional(bin_tarjeta varchar2,pBanco char )return pls_integer is
     vcont     pls_integer := 0;
     vlon_bin  pls_integer;



    cursor cur_dataNacional(bin_tarjeta varchar2) is
       select val_rango_menor, val_rango_mayor
         from bines_mc
        where (to_number(substr(val_rango_menor,1,6))        >= to_number(substr(bin_tarjeta,1,6))
          and  to_number(substr(val_rango_menor,1,vlon_bin)) <= to_number(rpad(substr(bin_tarjeta,1,vlon_bin),vlon_bin,9)))
          and  ind_activo                                     = 'A'
          and  cod_alf_pais                                   = 'VEN'
          and  cod_entadq                                     = pBanco
          and  ind_prog_tarjeta                               = 'MCC';



  begin

      vlon_bin:= length(bin_tarjeta);

      for c in cur_dataNacional(bin_tarjeta) loop
               if  bin_tarjeta between c.val_rango_menor  and c.val_rango_mayor  then
                    vcont       := vcont + 1;
               end if;
      end loop;


     if vcont > 1 then
        vErrMsg := 'No puede haber mas de 01 rango de valor para la tarjeta: '||bin_tarjeta;
        verrcod := '23';
        raise eFinError;
     end if;

     return vcont;
end;






function f_FindCodigoRazon(pCodRazon number, pNumTarjeta varchar2 )return varchar2 is
     vCodRazon  varchar2(4);
begin

    select lpad(c9codmc,4,0)
      into vCodRazon
      from codigos_de_razon_del_mensaje
     where p25codraz = pCodRazon;

   return vCodRazon;

exception
   when no_data_found  then
      verrmsg := 'No Existe codigo de Razon para tarjeta Nro '||pNumTarjeta;
      verrcod := '11';
      raise efinerror;


   when too_many_rows then
      verrmsg := 'Hay mas de 01 codigo de razon para tarjeta Nro '||pNumTarjeta;
      verrcod := '12';
      raise efinerror;

end;







function f_AtLeastOne(pfecha varchar2,pbanco varchar2)return pls_integer is
   vCountReg          pls_integer := 0;
   idepre             varchar2(6)      := '';
begin

   if pbanco = '0105' then idepre := '010375'; end if;
   if pbanco = '0108' then idepre := '010403'; end if;


      select count(idemen_p00_actc) into vCountReg
        from rechazos_retornos
       where fec_upd          = to_date(pfecha,'yyyymmdd')
         --and SUBSTR(ideadq_p32_actc,3,4)  = pbanco
         and (SUBSTR(ideadq_p32_actc,3,4)  = pbanco OR ( idemen_p00_actc = '1740' AND idepre_p33_actc  = idepre))
         and estado           = 'T'
         and tipo_incoming    = 'NA'; 
               -- IPR 1302 para este cambio sera necerio especificar el tipo_incoming para naiguata y se utilizaria la misma tabla
  return vCountReg;
end;





procedure p_findTransOriginal(pbanco varchar2, p31 varchar2,pNumTarjeta varchar2,
                               pImpTran out number, pMonTran out varchar2) is

begin
    --inicia busqueda de transaccion original
    pImpTran  := 0;
    pMonTran  := 0;

    if pbanco = 'BM' then
        select p04imptra, p49montra
          into pImpTran, pMonTran
          from mcp_bm
         where p31refadq  = p31
           and p00idmsg   = '1244';
    end if;

    if  pbanco = 'BP' then
         select p04imptra, p49montra
           into pImpTran, pMonTran
           from mcp_bp
          where p31refadq  = p31
            and p00idmsg   = '1244';
    end if;

exception
   when no_data_found  then
      verrmsg := 'No Existe transaccion original para tarjeta Nro '||pNumTarjeta;
      verrcod := '14';
      raise efinerror;


   when too_many_rows then
      verrmsg := 'Hay mas de 01 transaccion original para tarjeta Nro '||pNumTarjeta;
      verrcod := '15';
      raise efinerror;
end;







procedure p_findTransactionIdDisputas(p31 varchar2, pNumTarjeta varchar2, pTransationID out varchar2,
                                       pSourceAmount out varchar2,PcurrenCode out varchar2,Pinc_doc_ind out varchar2)is


 begin

       with tabla_01 as (
                 select substr(transaction_id,1,10)transaction_id,
                        lpad(incoming_source_amount,12,0)incoming_source_amount,
                        lpad(incoming_source_curren_code,3,0)incoming_source_curren_code,
                        nvl(trim(incoming_documentation_ind),'0') incoming_documentation_ind
                   from disputas_master
                  where incoming_refadq = p31
                    and disputa_stat    = 'T'
                    and (disputa_type   = '15' or disputa_type    = '16')
                  order by disputa_id desc)
        select transaction_id, incoming_source_amount, incoming_source_curren_code,incoming_documentation_ind
          into pTransationID,pSourceAmount,Pcurrencode, Pinc_doc_ind
          from tabla_01
         where rownum < 2;



exception
   when no_data_found then
      verrmsg := 'No hay registro REFADQ31 para tarjeta Nro '||pNumTarjeta;
      verrcod := '16';
      raise efinerror;
end;





procedure p_getDataInternacional(bin_tarjeta varchar2,pBanco char,pgcms out varchar2,plic_prod out varchar2,pcod_pais out varchar2,pcod_region out varchar2) is
   vlon_bin  pls_integer;
   vcont     pls_integer :=0;



    cursor cur_dataInternacional(bin_tarjeta varchar2) is
         select val_rango_menor, val_rango_mayor, cod_producto_gcms, cod_licencia_producto,cod_alf_pais, cod_region
           from bines_mc
          where (to_number(substr(val_rango_menor,1,6))        >= to_number(substr(bin_tarjeta,1,6))
            and  to_number(substr(val_rango_menor,1,vlon_bin)) <= to_number(rpad(substr(bin_tarjeta,1,vlon_bin),vlon_bin,9)))
            and  ind_activo                                     = 'A'
            and  cod_alf_pais                                  != 'VEN'
            and  cod_entadq                                     = pBanco
            and  ind_prog_tarjeta                               = 'MCC';

 begin
     vlon_bin:= length(bin_tarjeta);

    for c in cur_dataInternacional(bin_tarjeta) loop
         if  bin_tarjeta between c.val_rango_menor  and c.val_rango_mayor  then
              pgcms       := c.cod_producto_gcms;
              plic_prod   := c.cod_licencia_producto;
              pcod_pais   := c.cod_alf_pais;
              pcod_region := c.cod_region;
              vcont       := vcont + 1;
         end if;
    end loop;


    if vcont = 0 then
       verrmsg := 'No existe rango menor y mayor para la tarjeta Nro '||bin_tarjeta;
       verrcod := '17';
       raise efinerror;
    end if;

    if vcont > 1 then
       verrmsg := 'Hay mas de 01 rango menor y mayor para la tarjeta Nro '||bin_tarjeta;
       verrcod := '24';
       --raise efinerror;
    end if;


end;









procedure p_findird(pin_mcc varchar2, pout_mcc out varchar2, pird out pls_integer, ptcc out varchar2) is

begin

    select cod_mcc, mc_ird, cod_tcc
      into pout_mcc,pird, ptcc
      from mcc_pmp
     where cod_mcc = to_number(pin_mcc);


exception
    when no_data_found then
         verrmsg := 'No Existe [COD_MCC,COD_IRD,COD_TCC] para un registro';
         verrcod := '18';
         raise efinerror;
    when too_many_rows then
         verrmsg := 'Solo debe haber un codigo MCC para un establecimiento';
         verrcod := '19';
         raise efinerror;
end;









function f_IsDate(pstring varchar2)return boolean is
 ld_date   date;

begin
     ld_date := to_date(pstring,'yyyymmdd');
     return true;
exception
     when others then
        return false;
end;






function f_isBetweenFiveDay(pfecha date) return boolean is
   vi_fecha   pls_integer;

begin
      select count(*)
        into vi_fecha
        from dual
       where trunc(sysdate) between pfecha and pfecha + 5;
--       where pfecha between pfecha and pfecha + 5;

      if vi_fecha > 0 then
          return true;
      else
          return false;
      end if;

exception
     when others then
        return false;
end;


function f_isBetweenThirtyDay(pfecha date) return boolean is
   vi_fecha   pls_integer;

begin
      select count(*)
        into vi_fecha
        from dual
       where pfecha between pfecha and pfecha + 30;

      if vi_fecha > 0 then
          return true;
      else
          return false;
      end if;

exception
     when others then
        return false;
end;





function f_GenCheckDigit(pCadena varchar2) return pls_integer is
     suma     pls_integer := 0;
     x        pls_integer := 0;
     y        pls_integer := 0;
     numero   ArrayLuhn;

begin
     numero     := f_IniArrayLuhn;
     for i in 1..17 loop
         numero(i) := to_number(substr(pCadena,i,1));
     end loop;


     for x in 1..17 loop
         if mod(x,2) > 0 then   --impar
             y := numero(x)*2;
             if y >= 10 then
                y := y - 9;     --como sumar sus dos digitos
             end if;
         else
             y := numero(x);
         end if;
         suma := suma + y;
     end loop;

     suma := 10-(mod(suma,10));

     if suma = 10 then
        suma :=0;
     end if;

     return suma;
end;






function f_getIdBankDestino(bin_tarjeta varchar2,pBanco char)return varchar2 is
   vlon_bin                   pls_integer;
   vcod_mienbro               varchar2(11);
   vval_rango_menor           bines_mc.val_rango_menor%type;
   vval_rango_mayor           bines_mc.val_rango_mayor%type;
   vcod_miembro               bines_mc.cod_miembro%type;

 begin

    vlon_bin:= length(bin_tarjeta);
     select val_rango_menor,val_rango_mayor,cod_miembro
       into vval_rango_menor,vval_rango_mayor,vcod_miembro
       from bines_mc
      where (to_number(substr(val_rango_menor,1,6))        >= to_number(substr(bin_tarjeta,1,6))
        and  to_number(substr(val_rango_mayor,1,vlon_bin)) <= to_number(rpad(substr(bin_tarjeta,1,6),vlon_bin,9)))
        and  cod_entadq                                    = pBanco
        and  ind_activo                                    =   'A';


      if bin_tarjeta between vval_rango_menor and vval_rango_mayor then
           vcod_mienbro := lpad(to_char(vcod_miembro),11,0);
           return vcod_mienbro;
      end if;


exception
    when no_data_found then
         verrmsg := 'No existe el ID_INSTITUCION_DESTINO para la tarjeta '||bin_tarjeta;
         verrcod := '13';
         raise efinerror;

    when too_many_rows then
         verrmsg := 'Hay mas de 01 ID_INSTITUCION_DESTINO para la tarjeta '||bin_tarjeta;
         verrcod := '20';
         raise efinerror;

end;




procedure p_Insert_CtlProcMc(pIDProc number, pIdAdq varchar2,pEstado char,pNumSec number,pfecha date) is

    Begin

    insert  into ctl_procmc(id_funcion,
                            nom_proceso,
                            id_entadq,
                            estado ,
                            sec_outgoing,
                            FH_SESION)
                     values(pidproc,
                            'POUTREMC_NGTA',
                            pidadq,
                            pestado,
                            pNumSec ,
                            pfecha );

    Commit;

End;




--function f_getSecuencia(pbanco char, pNomProc char,pRepro char:='N',pfecha date) return pls_integer  is
function f_getSecuencia(pfecha date) return pls_integer  is
    vsecuence   pls_integer := 0;

    begin

/*    if pRepro = 'S' then
          select  nvl(max(sec_outgoing),0) + 1
          into  vsecuence
          from  ctl_procmc
         where  id_entadq    = pBanco
           and  nom_proceso  in ('POUTMC','POUTREMC')
           and  estado       = 'F'
--           and  fh_sesion    = trunc(sysdate);
           and  fh_sesion    = pfecha;
   elsif pRepro = 'N' then
          select nvl(max(sec_outgoing),0) -- decode(nvl(max(sec_outgoing),0),0,1,nvl(max(sec_outgoing),0))
            into  vsecuence
            from  ctl_procmc
           where  id_entadq   = pBanco
             and  nom_proceso = pNomProc
--             and  fh_sesion   = trunc(sysdate);
             and  fh_sesion   = pfecha;
   end if;*/
          select  nvl(max(sec_outgoing),0) + 1
          into  vsecuence
          from  ctl_procmc
         where  nom_proceso  in ('POUTMC_NGTA','POUTREMC_NGTA')
           and  fh_sesion    = pfecha
           and  estado       = 'F';

   return   vsecuence;

end;

end;
/


GRANT EXECUTE ON SGCVNZ.PQOUTRETORNOMC_NGTA TO ROLE_SOPTECN;
