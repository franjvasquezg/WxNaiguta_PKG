CREATE OR REPLACE PACKAGE SGCVNZ.PQPCLRRPT
IS

gRetN          NUMBER:=NULL;

PROCEDURE P_CLR_GENDATRPT_PLATCO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1));

FUNCTION F_CLR_RPTCUADRE (pEntAdq CHAR, pCodHrCierre CHAR,
                           pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR;

FUNCTION F_CLR_GENFILERPT (pCodReporte CHAR,
                           pFecSesion DATE:=TRUNC(SYSDATE-1),
                           pCodEntAdq CHAR ,
                           pCodMoneda CHAR:='862',
                           pCodHrCierre CHAR,
                           pTipoOut CHAR:='C' ) RETURN NUMBER;

FUNCTION F_CLR_GENDATRPT_PCLM (pCodEntAdq CHAR, pFecSesion DATE, pCodHrCierre CHAR) RETURN CHAR;

--PROCEDURE P_COMP_GENDATRPT0304 (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1));

FUNCTION F_GETIMPNETO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE, ptipmov CHAR , pCodMoneda CHAR) return number ;

FUNCTION f_GetNumero( ptipo         VARCHAR2,
                      pidemen_p00    NUMBER,
                      pcodpro_p03    VARCHAR2,
                      pimptra_p04    NUMBER,
                      psigcu1_p46    VARCHAR2,
                      pimpcu1_p46    NUMBER) RETURN NUMBER;


PROCEDURE P_COMP_GENDATRPT05   (pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1), pCodMoneda CHAR);

FUNCTION f_GetComisionMC  (pPDS0158_P48 CHAR, pP28SESION CHAR, pP04IMPTRA CHAR,
               pP48TIPTRA CHAR, pP18CODACT CHAR, pP48TIPMOV CHAR) RETURN NUMBER;

  FUNCTION F_COMP_GENFILERPT0204 (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion date, pTipoFile CHAR) RETURN CHAR;
  FUNCTION F_CLR_GENDATRPT (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR;
 -- FUNCTION P_COMP_GENDATRPT_PLATCO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR;
 PROCEDURE P_COMP_GENDATRPT_PLATCO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1));
  FUNCTION P_COMP_PLATCO_RETORNOS (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR;
  FUNCTION F_CLR_GENDATRPT_PCLM_HORA (pCodEntAdq CHAR, pFecSesion DATE, pCodHrCierre CHAR,phra_proceso CHAR) RETURN CHAR;
END;
/
GRANT EXECUTE ON SGCVNZ.PQPCLRRPT TO ROLE_SOPTECN;

CREATE OR REPLACE PACKAGE BODY SGCVNZ.PQPCLRRPT
IS

r                         SGDVNZ.RECHAZOS_RETORNOS%ROWTYPE;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- FUNCTION P_CLR_GENDATRPT
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION F_CLR_GENDATRPT (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR
IS

--
-- Proposito: Generar los datos en las tablas CLR_RESUMEN y CLR_RPTOUT102
--            luego de haberse efectuado el Proceso de Clearing
--
-- Persona     Fecha     Comentarios
-- ---------   --------  -------------------------------------------
-- SSM         20031114  Codigo Inicial
---JPC         20070415  Adapatacion a PMPV
--

/*

-- *****************************************************************************
-- Documento de Analisis del Clearing Visanet (MAG)
-- *****************************************************************************

CLR_LMTOTAS : Tabla de Registros de conciliacion del Clearing VISA Soles,
              desde el punto de vista del Merchant.
CLR_LETOTAS : Tabla de Registros de conciliacion del Clearing VISA Soles,
              desde el punto de vista del Emisor.
CLR_LMTOTAD : Tabla de Registros de conciliacion del Clearing VISA Dolares,
              desde el punto de vista del Merchant.
CLR_LETOTAD : Tabla de Registros de conciliacion del Clearing VISA Dolares,
              desde el punto de vista del Emisor.

numabo_s74_accc   : Numero de Devoluciones (abonos)
numana_s75_accc   : Numero de Anulaciones de Compra (abonos)
numcar_s76_accc   : Numero de Compras (cargos)
numanc_s77_accc   : Numero de Anulaciones de Devolucion (cargos)  (*)
impabo_s86_accc   : Importe de Devoluciones (abonos)
impana_s87_accc   : Importe de Anulaciones de Compra (abonos)
impcar_s88_accc   : Importe de Compras (cargos)
impanc_s89_accc   : Importe de Anulaciones de Devolucion (cargos) (*)
idedes_s93_accc   : Entidad Destino [substr(3,4)]
signox_s97_accc   : Si es 'D' indica saldo 'Deudor', se maneja como signo '-'
                    Si es 'C' indica saldo 'Acreedor',  se maneja como signo '+'
netoxx_s97_accc   : Es el monto neto para la entidad / lote, sin signo.
ichaab_s105_accc  : Importe de Abono de contracargos (abonos)
impanc_s106_accc  : Importe de Anulaciones de Devolucion (cargos)
nchaab_s107_accc  : Numero de Abono de contracargos (abonos)
nmpanc_s108_accc  : Numero de Anulaciones de Devolucion (cargos)
cuoabo_s109_acc   : Importe de Abonos de Comisiones
cuocar_s110_acc   : Importe de Cargos de Comisiones
nabomis_s111_accc : Numero de Abono Miscelaneos (Pagos) (abonos)
ncarmis_s112_accc : Numero de Cargos Miscelaneos (Cobros) (cargos)
abomis_s113_accc  : Importe de Abono Miscelaneos (Pagos) (abonos)
carmis_s114_accc  : Importe de Cargos Miscelaneos (Cobros) (cargos)

(*) Los campos existen pero sus valores SIEMPRE deben ser CERO.

-- *****************************************************************************
-- TABLA : CLR_RESUMEN
-- *****************************************************************************

Column Name        Null?    Type        Descripcion
--------------     -------- ----        ----------------------------
FEC_SESION         NOT NULL DATE        Fecha de Proceso
COD_HRCIERRE                CHAR(1 BYTE) Hora de Cierre 1=00:00 Horas/2=04:00 Horas
COD_MONEDA         NOT NULL CHAR(3)     604=Soles/840=Dolares
TIPO_LOTE          NOT NULL CHAR(1)     E=Emisor/M=Merchant
COD_ENTIDAD        NOT NULL NUMBER      Codigo del Banco (CSB)
TIPO_SALDO         NOT NULL CHAR(1)     D=Deudor/A=Acreedor
NUM_LOTE           NOT NULL VARCHAR2(3) Numero del Lote
NUM_OPER           NOT NULL NUMBER      Numero de Transacciones
IMP_NETO           NOT NULL NUMBER      Importe Neto
IMP_NETO_SIGNO     NOT NULL NUMBER      Importe Neto
NUMABO_S74_ACCC             NUMBER(10)
NUMANA_S75_ACCC             NUMBER(10)
NUMCAR_S76_ACCC             NUMBER(10)
NUMANC_S77_ACCC             NUMBER(10)
IMPABO_S86_ACCC             NUMBER(16)
IMPANA_S87_ACCC             NUMBER(16)
IMPCAR_S88_ACCC             NUMBER(16)
IMPANC_S89_ACCC             NUMBER(16)
ICHAAB_S105_ACCC            NUMBER(16)
ICHACA_S106_ACCC            NUMBER(16)
NCHAAB_S107_ACCC            NUMBER(10)
NCHACA_S108_ACCC            NUMBER(10)
CUOABO_S109_ACCC            NUMBER(16)
CUOCAR_S110_ACCC            NUMBER(16)
NABOMIS_S111_ACCC           NUMBER(10)
NCARMIS_S112_ACCC           NUMBER(10)
ABOMIS_S113_ACCC            NUMBER(16)
CARMIS_S114_ACCC            NUMBER(16)

*/

vCodMoneda      CHAR(3) := '862' ;
vDescMoneda     VARCHAR2(20) := 'Bolivares';

vIdProc         NUMBER;
vRetC           VARCHAR2(100);
vOraCode        NUMBER;
vOraErr         VARCHAR2(200);
vErrMsg         VARCHAR2(200);
vErrCod         VARCHAR2(2);
vPaso           VARCHAR2(10);

PROCEDURE InsDatRptRes
IS
BEGIN
    -- Elimina informacion anterior
    pqmonproc.inslog(vIdProc, 'M', 'Eliminando informacion anterior de clr_resumen.');
    DELETE CLR_RESUMEN
      WHERE fec_sesion  = pFecSesion
       AND cod_hrcierre = pCodHrCierre
       AND cod_entadq   = pEntAdq;

    -- Emisor
    pqmonproc.inslog(vIdProc, 'M', 'Procesando informacion de Emisor.');
    FOR r IN (SELECT SUBSTR(idedes_s93_accc,3,4) EntDest, signox_s97_accc,
                     inlote_p29_accc,
                     NVL((numabo_s74_accc),0)+NVL((numana_s75_accc),0)+
                     NVL((numcar_s76_accc),0)+NVL((numanc_s77_accc),0)+
                     NVL((nchaab_s107_accc),0)+NVL((nchaca_s108_accc),0)+
                     NVL((nabomis_s111_accc),0)+NVL((ncarmis_s112_accc),0) NumOpe,
                     NVL((netoxx_s97_accc),0) ImpNeto,
                     NVL((DECODE(signox_s97_accc,'D',(-1)*netoxx_s97_accc,'C',netoxx_s97_accc)),0) ImpNetoSigno,
                     NVL((numabo_s74_accc),0) numabo_s74_accc,
                     NVL((numana_s75_accc),0) numana_s75_accc,
                     NVL((numcar_s76_accc),0) numcar_s76_accc,
                     NVL((numanc_s77_accc),0) numanc_s77_accc,
                     NVL((impabo_s86_accc),0) impabo_s86_accc,
                     NVL((impana_s87_accc),0) impana_s87_accc,
                     NVL((impcar_s88_accc),0) impcar_s88_accc,
                     NVL((impanc_s89_accc),0) impanc_s89_accc,
                     NVL((ichaab_s105_accc),0) ichaab_s105_accc,
                     NVL((ichaca_s106_accc),0) ichaca_s106_accc,
                     NVL((nchaab_s107_accc),0) nchaab_s107_accc,
                     NVL((nchaca_s108_accc),0) nchaca_s108_accc,
                     NVL((cuoabo_s109_accc),0) cuoabo_s109_accc,
                     NVL((cuocar_s110_accc),0) cuocar_s110_accc,
                     NVL((nabomis_s111_accc),0) nabomis_s111_accc,
                     NVL((ncarmis_s112_accc),0) ncarmis_s112_accc,
                     NVL((abomis_s113_accc),0) abomis_s113_accc,
                     NVL((carmis_s114_accc),0) carmis_s114_accc,
                     tipmov, moneda_pxxx_accc
                FROM CLR_LETOTA
               WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                 AND cod_hrcierre = pCodHrCierre
                 AND cod_entadq = pEntAdq
            ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
    LOOP
        INSERT INTO clr_resumen
            (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entadq,
             cod_entidad, num_lote, tipo_saldo, num_oper,
             imp_neto, imp_neto_signo, tipo_saldo_comp, num_oper_comp,
             imp_neto_comp, imp_neto_signo_comp, numabo_s74_accc,
             numana_s75_accc, numcar_s76_accc, numanc_s77_accc,
             impabo_s86_accc, impana_s87_accc, impcar_s88_accc,
             impanc_s89_accc, ichaab_s105_accc, ichaca_s106_accc,
             nchaab_s107_accc, nchaca_s108_accc, cuoabo_s109_accc,
             cuocar_s110_accc, nabomis_s111_accc, ncarmis_s112_accc,
             abomis_s113_accc, carmis_s114_accc, tipmov)
        VALUES
            (pfecsesion, pcodhrcierre, r.moneda_pxxx_accc, 'E', pentadq,
             r.entdest, r.inlote_p29_accc, r.signox_s97_accc, r.numope,
             r.impneto, r.impnetosigno, r.signox_s97_accc, r.numope,
             r.impneto, r.impnetosigno, r.numabo_s74_accc,
             r.numana_s75_accc, r.numcar_s76_accc, r.numanc_s77_accc,
             r.impabo_s86_accc, r.impana_s87_accc, r.impcar_s88_accc,
             r.impanc_s89_accc, r.ichaab_s105_accc, r.ichaca_s106_accc,
             r.nchaab_s107_accc, r.nchaca_s108_accc, r.cuoabo_s109_accc,
             r.cuocar_s110_accc, r.nabomis_s111_accc, r.ncarmis_s112_accc,
             r.abomis_s113_accc, r.carmis_s114_accc, r.tipmov);
     END LOOP;

     -- Merchant
     pqmonproc.inslog(vIdProc, 'M', 'Procesando informacion de Merchant.');
     FOR r IN (SELECT SUBSTR(idedes_s93_accc,3,4) EntDest, signox_s97_accc,
                      inlote_p29_accc,
                      NVL((numabo_s74_accc),0)+NVL((numana_s75_accc),0)+
                      NVL((numcar_s76_accc),0)+NVL((numanc_s77_accc),0)+
                      NVL((nchaab_s107_accc),0)+NVL((nchaca_s108_accc),0)+
                      NVL((nabomis_s111_accc),0)+NVL((ncarmis_s112_accc),0) NumOpe,
                      NVL((netoxx_s97_accc),0) ImpNeto,
                      NVL((DECODE(signox_s97_accc,'D',(-1)*netoxx_s97_accc,'C',netoxx_s97_accc)),0) ImpNetoSigno,
                      NVL((numabo_s74_accc),0) numabo_s74_accc,
                      NVL((numana_s75_accc),0) numana_s75_accc,
                      NVL((numcar_s76_accc),0) numcar_s76_accc,
                      NVL((numanc_s77_accc),0) numanc_s77_accc,
                      NVL((impabo_s86_accc),0) impabo_s86_accc,
                      NVL((impana_s87_accc),0) impana_s87_accc,
                      NVL((impcar_s88_accc),0) impcar_s88_accc,
                      NVL((impanc_s89_accc),0) impanc_s89_accc,
                      NVL((ichaab_s105_accc),0) ichaab_s105_accc,
                      NVL((ichaca_s106_accc),0) ichaca_s106_accc,
                      NVL((nchaab_s107_accc),0) nchaab_s107_accc,
                      NVL((nchaca_s108_accc),0) nchaca_s108_accc,
                      NVL((cuoabo_s109_accc),0) cuoabo_s109_accc,
                      NVL((cuocar_s110_accc),0) cuocar_s110_accc,
                      NVL((nabomis_s111_accc),0) nabomis_s111_accc,
                      NVL((ncarmis_s112_accc),0) ncarmis_s112_accc,
                      NVL((abomis_s113_accc),0) abomis_s113_accc,
                      NVL((carmis_s114_accc),0) carmis_s114_accc,
                      tipmov, moneda_pxxx_accc
                 FROM CLR_LMTOTA
                WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                  AND cod_hrcierre = pCodHrCierre
                  AND cod_entadq = pEntAdq
             ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
     LOOP
        INSERT INTO clr_resumen
            (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entadq,
             cod_entidad, num_lote, tipo_saldo, num_oper,
             imp_neto, imp_neto_signo, tipo_saldo_comp, num_oper_comp,
             imp_neto_comp, imp_neto_signo_comp, numabo_s74_accc,
             numana_s75_accc, numcar_s76_accc, numanc_s77_accc,
             impabo_s86_accc, impana_s87_accc, impcar_s88_accc,
             impanc_s89_accc, ichaab_s105_accc, ichaca_s106_accc,
             nchaab_s107_accc, nchaca_s108_accc, cuoabo_s109_accc,
             cuocar_s110_accc, nabomis_s111_accc, ncarmis_s112_accc,
             abomis_s113_accc, carmis_s114_accc, tipmov)
        VALUES
            (pfecsesion, pcodhrcierre, r.moneda_pxxx_accc, 'M', pentadq,
             r.entdest, r.inlote_p29_accc, r.signox_s97_accc, r.numope,
             r.impneto, r.impnetosigno, r.signox_s97_accc, r.numope,
             r.impneto, r.impnetosigno, r.numabo_s74_accc,
             r.numana_s75_accc, r.numcar_s76_accc, r.numanc_s77_accc,
             r.impabo_s86_accc, r.impana_s87_accc, r.impcar_s88_accc,
             r.impanc_s89_accc, r.ichaab_s105_accc, r.ichaca_s106_accc,
             r.nchaab_s107_accc, r.nchaca_s108_accc, r.cuoabo_s109_accc,
             r.cuocar_s110_accc, r.nabomis_s111_accc, r.ncarmis_s112_accc,
             r.abomis_s113_accc, r.carmis_s114_accc, r.tipmov);
    END LOOP;
END;

BEGIN

    /* ID DE PROCESO
    ****************/
    vIdProc:= pqmonproc.InsMonProc ('PCLRDATRPT');

    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre );
    pqmonproc.inslog(vIdProc, 'M', 'Insertando datos en clr_resumen.');
    InsDatRptRes;

    -- Genera Datos Platco
    pqmonproc.inslog(vIdProc, 'M', 'Insertando datos en clr_resumen_platco.');
    P_CLR_GENDATRPT_PLATCO (pEntAdq, pCodHrCierre, pFecSesion);

    -- Compensacion Platco /*Se comenta por retardo del archivo del BP*/
--    pqmonproc.inslog(vIdProc, 'M', 'Insertando datos de Compensacion Platco.');
--    P_COMP_GENDATRPT_PLATCO (pEntAdq, pCodHrCierre, pFecSesion);

    /* FIN DE PROCESO
    *****************/
    vRetC  := pqmonproc.updmonproc (vIdProc, 'F');
    RETURN '0~';

EXCEPTION
    WHEN others THEN
         ROLLBACK;
         voracode := abs(sqlcode);
         voraerr  := substr(sqlerrm,1,200);
         pqmonproc.InsLog(vIDProc, 'E', vOraErr);
         vretc    := pqmonproc.updmonproc (vIdProc, 'E', '1');
         RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END; -- P_CLR_GENDATRPT


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- FUNCTION F_CLR_RPTCUADRE
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION F_CLR_RPTCUADRE (pEntAdq CHAR, pCodHrCierre CHAR,
                           pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR
IS

vDirOUT       VARCHAR2(100);
vFileRPT      VARCHAR2(50);

vImpNetoA     NUMBER:=0;  -- Importe Acreedor Por Lote
vImpNetoD     NUMBER:=0;  -- Importe Deudor Por Lote
vTotDeudor    NUMBER:=0;  -- Total Deudor
vTotAcreedor  NUMBER:=0;  -- Total Acreedor

vCodLCSB      ENTIDADES_PRICE.c00lcsb%TYPE;
vDescEntAdq   ENTIDADES_PRICE.c00nredu%TYPE;
vBinADQ       VARCHAR2(6);
vFlgCuadre    CHAR(1):='S';
vDummyTXT     VARCHAR2(80);
vIDFile       UTL_FILE.FILE_TYPE;

vIdProc       NUMBER;
vRetC         VARCHAR2(100);
vOraCode      NUMBER;
vOraErr       VARCHAR2(200);
vErrMsg       VARCHAR2(200);
vErrCod       VARCHAR2(2);
vPaso         VARCHAR2(10);
eFinError     EXCEPTION;

--INI - Se calcula la suma de montos de las txs premiadas (anulaciones) de la tabla mx8000.
vImpTxsPrem   NUMBER:=0;  -- Importe total de las txs premiadas en la tabla mx8000.
--FIN - Se calcula la suma de montos de las txs premiadas (anulaciones) de la tabla mx8000.


PROCEDURE GetDatos (pTipoLote CHAR)
IS
BEGIN
      -- DEUDOR
      SELECT NVL(SUM(NVL(IMP_NETO,0)),0) IMP_NETO
        INTO vImpNetoD
        FROM CLR_RESUMEN
       WHERE FEC_SESION = pFecSesion
--         AND COD_MONEDA = pCodMon
         AND TIPO_LOTE = pTipoLote
         AND COD_HRCIERRE = pCodHrCierre
         AND TIPO_SALDO = 'D'
         AND COD_ENTADQ = pEntAdq;

      -- ACREEDOR
      SELECT NVL(SUM(NVL(IMP_NETO,0)),0) IMP_NETO
        INTO vImpNetoA
        FROM CLR_RESUMEN
       WHERE FEC_SESION = pFecSesion
--         AND COD_MONEDA = pCodMon
         AND TIPO_LOTE = pTipoLote
         AND COD_HRCIERRE = pCodHrCierre
         AND TIPO_SALDO = 'C'
         AND COD_ENTADQ = pEntAdq;
END; -- GetTotales

PROCEDURE pPutTXT(pTexto CHAR, pCentrar CHAR:='N',pChar CHAR:='*')
IS
vTexto VARCHAR2(100):=pTexto;
vDummy NUMBER;
BEGIN
  IF vTexto = '*' THEN
     vTexto:='********************************************************************************';
  ELSIF vTexto = '-' THEN
     vTexto:='--------------------------------------------------------------------------------';
  ELSIF vTexto = '#' THEN
     vTexto:='      ####################################################################';
  ELSE
     IF pCentrar = 'S' THEN
        IF MOD(LENGTH(vTexto),2) <> 0 THEN
           vTexto:=vTexto||' ';
        END IF;
        vDummy:=((80-(LENGTH(pChar))*2)-(LENGTH(vTexto)))/2;
        vTexto:=pChar||LPAD(' ',vDummy,' ')||vTexto||LPAD(' ',vDummy,' ')||pChar;
     END IF;
  END IF;
  --dbms_output.put_line(vTexto);
  UTL_FILE.PUT(vIDFile,vTexto);UTL_FILE.NEW_LINE(vIDFile);
END;

FUNCTION num2char (pValNum NUMBER,pAlineacion CHAR:='D') RETURN CHAR
IS
vRet VARCHAR2(20);
BEGIN
   IF pValNum = 0 THEN
         vRet:='                0.00';
   ELSE
      IF pAlineacion = 'D' THEN
         vRet:=LPAD(LTRIM(TO_CHAR(pValNum/100,'9,999,999,999,999.99')),20,' ');
      ELSE
         vRet:=RPAD(LTRIM(TO_CHAR(pValNum/100,'9,999,999,999,999.99')),20,' ');
      END IF;
   END IF;
   RETURN vret;
END;

BEGIN
   vPaso := 'Paso 1';
   vIDProc := PQMONPROC.InsMonProc ('CLRRC');
   -- Directorio de Salida (DIR_OUT en STD_PARAMETRO)
   vDirOUT:=STD.F_GETVALPAR('DIR-OUT');
   IF vDirOUT = '' OR vDirOUT IS NULL THEN
      vErrMsg:='ERROR | Directorio de Salida (DIR_OUT) no definido en STD_PARAMETRO.';
      vErrCod := '2';
      RAISE eFinError;
   END IF;
   -- Validacion de la Hora de Cierre
   IF pCodHrCierre NOT IN ('1','2') THEN
      vErrMsg:='ERROR | Hora de Cierre Incorrecta [1=00:00 Horas/2=04:00 Horas]';
      vErrCod:= '3';
      RAISE eFinError;
   END IF;

   vPaso := 'Paso 2';
   -- Nombre del Archivo
   SELECT 'CLR'||pEntAdq||TO_CHAR(pFecSesion,'YYYYMMDD')||pCodHrCierre||'.RPT'
     INTO vFileRPT
     FROM DUAL;

   SELECT C00NREDU,
          C00LCSB
     INTO vDescEntAdq,
          vCodLCSB
     FROM ENTIDADES_PRICE
    WHERE COD_ENTADQ = pEntAdq
      AND c00ltipo = 0;

    -- BIN Adquirente
/*   SELECT DECODE(pCodMon,'604','491955','423632')
     INTO vBinADQ
     FROM DUAL; */

   vPaso := 'Paso 3';
   -- Abre el Archivo de Reporte
      vIDFile:=UTL_FILE.FOPEN(vDirOUT,vFileRPT,'W');
   IF NOT UTL_FILE.IS_OPEN(vIDFile) THEN
      --PutMsgOpe('Error al Abrir <'||vDirOUT||'/'||vFileRPT||'>');
      vErrMsg:='ERROR | Archivo <'||vFileRPT||'> NO Creado.';
      VErrCod:= '4';
      RAISE eFinError;
   END IF;

   vPaso := 'Paso 3.1';
   pPutTXT(' ');
   pPutTXT('*');
   pPutTXT('SISTEMA DE GESTION DE COMERCIOS','S');
   pPutTXT('REPORTE DE CUADRE DE CLEARING','S');
   pPutTXT('*');

   vPaso := 'Paso 3.2';
   SELECT 'Entidad: '||RPAD(vDescEntAdq,43,' ')||' Fecha de Sesion: '||TO_CHAR(pFecSesion,'DD/MM/YYYY')
     INTO vDummyTXT
     FROM DUAL;
   pPutTXT(vDummyTXT);
   SELECT RPAD(' ',54,' ')||'Hora de Cierre: '||DECODE(pCodHrCierre,1,'00:00 hrs.',2,'04:00 hrs.')
     INTO vDummyTXT
     FROM DUAL;
   pPutTXT(vDummyTXT);
   pPutTXT('-');
   pPutTXT(' ');

   vPaso := 'Paso 3.4';
   SELECT '                                      IMPORTE NETO (Bs/.)'
     INTO vDummyTXT
     FROM DUAL;

   vPaso := 'Paso 3.5';
   pPutTXT(vDummyTXT);
   pPutTXT('                         *******************************************');
   pPutTXT('           LOTE                 DEUDOR                ACREEDOR');
   pPutTXT('           -----------   --------------------   --------------------');

   vPaso := 'Paso 4';
   GetDatos('E');
   vTotDeudor:=vImpNetoD;
   vTotAcreedor:=vImpNetoA;
   pPutTXT('           EMISOR        '||num2char(vImpNetoD)||'   '||num2char(vImpNetoA));

   vPaso := 'Paso 5';
   GetDatos('M');   
   
   --INI - Se calcula la suma de montos de las txs premiadas (anulaciones) de la tabla mx8000.
   select sum(imptra_p04_actc) into vImpTxsPrem 
    from clr_mx8000
    where sesion_p28_actc =  TO_CHAR(pFecSesion,'YYMMDD')
       and cod_hrcierre  = pCodHrCierre
       AND SUBSTR(ideadq_p32_actc,3,4) IN (SELECT C00LCSB
                                             FROM ENTIDADES_PRICE
                                            WHERE COD_ENTADQ = pEntAdq
                                              AND C00LTIPO = 0)
       and codpre_p48_actc <> '00';
       
   if vImpTxsPrem is not null then
       vImpNetoA:=vImpNetoA-vImpTxsPrem;
   end if;
   --FIN - Se calcula la suma de montos de las txs premiadas (anulaciones) de la tabla mx8000.
  
   
   vTotDeudor:=vTotDeudor+vImpNetoD;
   vTotAcreedor:=vTotAcreedor+vImpNetoA;
   pPutTXT('           MERCHANT      '||num2char(vImpNetoD)||'   '||num2char(vImpNetoA));
   pPutTXT('                         --------------------   --------------------');
   pPutTXT('                         '||num2char(vTotDeudor)||'   '||num2char(vTotAcreedor));

   vPaso := 'Paso 6';
   IF (vTotAcreedor-vTotDeudor) = 0  THEN
      pPutTXT(' ');
      pPutTXT('-');
      pPutTXT('PROCESO DE CLEARING FINALIZADO CORRECTAMENTE','S','--');
   ELSE
      -- Descuadre
      vFlgCuadre:='N';
      pPutTXT(' ');
      pPutTXT('#');
      pPutTXT('      ###           DESCUADRE EN PROCESO DE CLEARING                   ###');
      pPutTXT('      ###                  Diferencia: '||num2char(vTotAcreedor-vTotDeudor,'I')||'                  ###');
      pPutTXT('#');
      pPutTXT(' ');
   END IF;

   vPaso := 'Paso 7';
   pPutTXT('-');
   pPutTXT('Telefonica Servicios Transaccionales'||LPAD(TO_CHAR(SYSDATE,'DD/MM/YYYY')||' '||TO_CHAR(SYSDATE,'HH24:MI:SS'),44,' '));
   pPutTXT(' ');
   UTL_FILE.FCLOSE(vIDFile);

   IF vFlgCuadre = 'N' THEN
      vDummyTXT:=pEntAdq||'-'||pCodHrCierre;
      PQMONPROC.InsLog(vIDProc, 'E', 'Descuadre en Proceso de Clearing '||vDummyTXT);
      vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
      --vRetN:=PQSGCMON.InsAlerta('SGCVI-MP'||STD.F_GETVALPAR('DB-INSTANCIA'),'Descuadre en Proceso de Clearing '||vDummyTXT||'. Avisar a Sistemas.');
      RETURN '1';
   ELSE
      PQMONPROC.InsLog(vIDProc, 'M', 'Termino proceso, archivo: '||vDirOUT||'/'||vFileRPT);
      vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'F');
      RETURN '0';
   END IF;

EXCEPTION
    WHEN eFinError THEN
         UTL_FILE.FCLOSE(vIDFile);
         PQMONPROC.InsLog(vIDProc, 'E', vErrMsg);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', vErrCod);
         RETURN vErrCod||vErrMsg;
    WHEN OTHERS THEN
         vOraCode:=abs(SQLCODE);
         UTL_FILE.FCLOSE(vIDFile);
         vErrMsg:='Error de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')';
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '4');
         RETURN vErrCod||vErrMsg;
END; -- F_CLR_RPTCUADRE


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- F_CLR_GENFILERPT
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION F_CLR_GENFILERPT (pCodReporte CHAR,
                           pFecSesion DATE:=TRUNC(SYSDATE-1),
                           pCodEntAdq CHAR ,
                           pCodMoneda CHAR:='862',
                           pCodHrCierre CHAR,
                           pTipoOut CHAR:='C' ) RETURN NUMBER
IS

/*
     Valores de los Parametros
     ---------------------------
     pCodReporte : OUT101/OUT102
     pCodEntAdq  : 0105, 0108
     pTipoOut    : C=Cliente/S=Servidor
*/

vIDrpt          NUMBER;
vNumLinea       NUMBER:=0;
vFecRPT         DATE:=TRUNC(SYSDATE);
vHoraRPT        CHAR(8):=TO_CHAR(SYSDATE,'HH24:MI:SS');

vNomMon         CHAR(9);
vSimbMon        CHAR(4);
vIDent          CHAR(6);
vPagina         NUMBER:=1;
vCont           NUMBER;
vTitRpt         VARCHAR2(80);
vTotDeudor      NUMBER:=0;
vTotAcreedor    NUMBER:=0;
vCntLotes       NUMBER:=0;
vLotesXPag      NUMBEr:=3;  -- Controla el Numero de Lotes Por Pagina

vDirOUT         VARCHAR2(100);
vFileRPT        VARCHAR2(50);
vIDFile         UTL_FILE.File_Type;
vOraErr         VARCHAR2(200);
vErrMsg         VARCHAR2(200);
vPaso           VARCHAR2(3);
eFinError       EXCEPTION;

cursor CurRpt101 is
select cod_entidad, c00nredu nom_entidad,
       nvl(sum(decode(tipo_saldo,'D',imp_neto,0)),0) sld_deudor,
       nvl(sum(decode(tipo_saldo,'C',imp_neto,0)),0) sld_acreedor
  from clr_resumen clr,
       entidades_price ent
 where clr.cod_entidad = ent.c00lcsb
   and fec_sesion = pFecSesion
   and cod_moneda = pCodMoneda
   and cod_hrcierre = pCodHrCierre
   and clr.cod_entadq  = pCodEntAdq
 group by cod_entidad, c00nredu
 order by cod_entidad;

cursor CurRpt102 is
select cod_entidad
       ,clr.num_lote num_lote
       ,c00nredu nom_entidad
       ,lot.nom_lote nom_lote
       ,nvl(numabo_s74_accc,0)      numabo_s74_accc
       ,nvl(numana_s75_accc,0)      numana_s75_accc
       ,nvl(numcar_s76_accc,0)      numcar_s76_accc
       ,nvl(numanc_s77_accc,0)      numanc_s77_accc
       ,nvl(impabo_s86_accc,0)      impabo_s86_accc
       ,nvl(impana_s87_accc,0)      impana_s87_accc
       ,nvl(impcar_s88_accc,0)      impcar_s88_accc
       ,nvl(impanc_s89_accc,0)      impanc_s89_accc
       ,nvl(ichaab_s105_accc,0)     ichaab_s105_accc
       ,nvl(ichaca_s106_accc,0)     ichaca_s106_accc
       ,nvl(nchaab_s107_accc,0)     nchaab_s107_accc
       ,nvl(nchaca_s108_accc,0)     nchaca_s108_accc
       ,nvl(cuoabo_s109_accc,0)     cuoabo_s109_accc
       ,nvl(cuocar_s110_accc,0)     cuocar_s110_accc
       ,nvl(nabomis_s111_accc,0)    nabomis_s111_accc
       ,nvl(ncarmis_s112_accc,0)    ncarmis_s112_accc
       ,nvl(abomis_s113_accc,0)     abomis_s113_accc
       ,nvl(carmis_s114_accc,0)     carmis_s114_accc
       ,nvl(imp_neto_signo,0)       imp_neto_signo
  from clr_resumen clr,
       entidades_price ent,
       clr_lotes lot
 where clr.cod_entidad = ent.c00lcsb
   and clr.num_lote = lot.num_lote
   and fec_sesion = pFecSesion
   and cod_moneda = pCodMoneda
   and cod_hrcierre = pCodHrCierre
   and clr.cod_entadq  = pCodEntAdq
 order by cod_entidad, clr.num_lote;

procedure PutTXT (pTexto CHAR) is
begin
  IF pTipoOut = 'S' THEN
     UTL_FILE.PUT(vIDFile,pTexto);UTL_FILE.New_Line(vIDFile);
  ELSE
     vNumLinea:=vNumLinea+1;
   /*  insert into pmp_reportes_out (id_reporte,
                                   fec_reporte,cod_reporte,
                                   num_fila,texto)
          values (vIDrpt,vFecRPT,pCodReporte,vNumLinea,pTexto); */
  END IF;
end;

function Fill (pNumEsp NUMBER,pChar CHAR:=' ') return char is
vRet  VARCHAR2(80):='';
begin
   for r in 1..pNumEsp loop
       vRet:=vRet||pChar;
   end loop;
   return vRet;
end;

function num2char (pValNum NUMBER,pNumEsp NUMBER,
                   pChar CHAR:=' ',pFlgImp CHAR:='N') return char
is
vRet varchar2(80);
begin
   if pFlgImp = 'S' then
      if pValNum = 0 then
         vRet:=lpad('0.00',pNumEsp,pChar);
      else
         vRet:=lpad(ltrim(to_char(pValNum/100,'999,999,999.99')),pNumEsp,pChar);
      end if;
   else
      vRet:=lpad(ltrim(to_char(pValNum,'999,999,999')),pNumEsp,pChar);
   end if;
   return vret;
end;

function char2campo (pTexto CHAR,pNumEsp NUMBER,pChar CHAR:=' ',pAlineacion CHAR:='I') return char
is
vRet varchar2(80);
begin
   if pAlineacion = 'I' then
      vRet:=rpad(ltrim(pTexto),pNumEsp,pChar);
   else
      vRet:=lpad(ltrim(pTexto),pNumEsp,pChar);
   end if;
   return vret;
end;

procedure PutCAB is
vDummy  varchar2(80);
begin
   -- Centra el Titulo del Reporte
   vDummy:=(80-(length(vTitRpt)))/2;
   vTitRpt:=LPAD(' ',vDummy,' ')||vTitRpt||LPAD(' ',vDummy,' ');
   -- Inserta la Cabecera del Reporte
   PutTXT(' ');
   PutTXT('PMPSGC - Sistema de Gestion de Comercios                     Fecha  : '||to_char(vFecRPT,'DD/MM/YYYY'));
   PutTXT('Proceso de Clearing                                          Hora   :   '||vHoraRPT);
   PutTXT('                                                             Pagina : '||lpad(vPagina,10,' '));
   PutTXT(vTitRpt);
   PutTXT(' ');
   PutTXT('Fecha de Sesion: '||to_char(pFecSesion,'DD/MM/YYYY')||'       Moneda: '||vNomMon||'       Reporte: '||pCodReporte||'-'||vIDent);
   PutTXT(fill(80,'_'));
   PutTXT(' ');
   PutTXT(' ');
end;

procedure PutLOTE (pEntidad CHAR, pLote CHAR)
is
begin
   PutTXT(' ENTIDAD: '||RPAD(pEntidad,45,' ')||' '||LPAD('LOTE: '||pLote,23,' '));
   PutTXT(' ------------------------------------------------------------------------------');
   PutTXT(' ');
end;


BEGIN
    -- Verifica que exista informacion para la Fecha de Sesion y Moneda
    vPaso := '01';
    SELECT COUNT(*)
      INTO vCont
      FROM CLR_RESUMEN
     WHERE FEC_SESION = pFecSesion
       AND COD_MONEDA = pCodMoneda
       AND cod_entadq = pCodEntAdq;
    IF vCont = 0 THEN
       -- No hay informacion
       RETURN 0;
    END IF;

    vPaso := '01a';
    -- Moneda
    IF pCodMoneda = '937' THEN
       vNomMon:='Bolivares';
       vSimbMon:='Bs/.';
       vIDent:='491955';
    ELSIF pCodMoneda = '840' THEN
          vNomMon:='Dolares';
          vSimbMon:='US$';
          vIDent:='423632';
    END IF;

    vPaso := '02';
    IF pTipoOut = 'S' THEN
       -- Se genera Archivo en el SERVIDOR
       vDirOUT:=STD.F_GETVALPAR('DIR-OUT');
       IF vDirOUT = '' OR vDirOUT IS NULL THEN
          vErrMsg:='FIN ERROR | Directorio de Salida (DIR_OUT) no definido en STD_PARAMETRO.';
          raise eFinError;
       END IF;
       -- Abre el Archivo de Reporte

       SELECT 'CLR'||pCodEntAdq||pCodReporte||TO_CHAR(pFecSesion,'YYYYMMDD')||pCodHrCierre||'.RPT'
         INTO vFileRPT
         FROM DUAL;

       dbms_output.put_line('vDirOUT:'||vDirOUT);
       dbms_output.put_line('vFileRPT:'||vFileRPT);
       vIDFile:=UTL_FILE.FOPEN(vDirOUT,vFileRPT,'W');
       IF NOT UTL_FILE.IS_OPEN(vIDFile) THEN
          vErrMsg:='FIN ERROR | No se pudo abrir el Archivo de Reporte.';
          raise eFinError;
       END IF;


    ELSE
       -- Se genera Archivo en la PC Cliente
       SELECT SEQ_ID_PMPRPTOUT.NEXTVAL INTO vIDrpt FROM DUAL;
    END IF;


    -- RPTOUT101
    IF pCodReporte = 'OUT101' THEN
       vPaso := '03';
       -- Cabecera
       vTitRpt:='RESUMEN DE COMPENSACION NETA';
       PutCAB;
       -- Detalle
       PutTXT(' ');
       PutTXT(' ');
       PutTXT('      ENTIDAD                              SALDO DEUDOR      SALDO ACREEDOR');
       PutTXT('      --------------------------------   ----------------   ----------------');
       PutTXT(' ');
       FOR r IN CurRpt101 LOOP
           PutTXT('      '||char2campo(lpad(r.cod_entidad,4,'0')||' - '||r.nom_entidad,32)||'   '||num2char(r.sld_deudor,16,' ','S')||'   '||num2char(r.sld_acreedor,16,' ','S'));
           vTotDeudor:=vTotDeudor+r.sld_deudor;
           vTotAcreedor:=vTotAcreedor+r.sld_acreedor;
       END LOOP;
       PutTXT('                                         ----------------   ----------------');
       PutTXT(char2campo('TOTALES ('||vSimbMon||')',38,' ','D')||'   '||num2char(vTotDeudor,16,' ','S')||'   '||num2char(vTotAcreedor,16,' ','S'));
    END IF;

    -- RPTOUT102
    IF pCodReporte = 'OUT102' THEN

       vPaso := '04';
       vTitRpt:='RESUMEN DE LIQUIDACION';
       FOR r IN CurRpt102 LOOP

           IF vCntLotes = 0 THEN
              PutCAB;
              PutLOTE(lpad(r.cod_entidad,4,'0')||'-'||r.nom_entidad,r.num_lote||' '||r.nom_lote);
           ELSE
              PutTXT(' ');
              PutTXT(' ');
              PutLOTE(lpad(r.cod_entidad,4,'0')||'-'||r.nom_entidad,r.num_lote||' '||r.nom_lote);
           END IF;

           PutTXT(' ');
           PutTXT('                                           Operaciones      Importe (Bs/.)');
           PutTXT('                                          -------------   -----------------');
           PutTXT('    (S074/S086) DEVOLUCIONES              '||num2char(r.numabo_s74_accc,13)||'   '||num2char(r.impabo_s86_accc,17,' ','S'));
           PutTXT('    (S075/S087) ANULACION COMPRAS, EFE    '||num2char(r.numana_s75_accc,13)||'   '||num2char(r.impana_s87_accc,17,' ','S'));
           PutTXT('    (S076/S088) COMPRAS, EFECTIVO         '||num2char(r.numcar_s76_accc,13)||'   '||num2char(r.impcar_s88_accc,17,' ','S'));
           PutTXT('    (S077/S089) ANULACION DEVOLUCIONES    '||num2char(r.numanc_s77_accc,13)||'   '||num2char(r.impanc_s89_accc,17,' ','S'));
           PutTXT('    (S107/S105) ABONO DE CONTRACARGOS     '||num2char(r.nchaab_s107_accc,13)||'   '||num2char(r.ichaab_s105_accc,17,' ','S'));
           PutTXT('    (S108/S106) CARGO DE CONTRACARGOS     '||num2char(r.nchaca_s108_accc,13)||'   '||num2char(r.ichaca_s106_accc,17,' ','S'));
           PutTXT('    (S111/S113) ABONOS MISCELANEOS        '||num2char(r.nabomis_s111_accc,13)||'   '||num2char(r.abomis_s113_accc,17,' ','S'));
           PutTXT('    (S112/S114) CARGOS MISCELANEOS        '||num2char(r.ncarmis_s112_accc,13)||'   '||num2char(r.carmis_s114_accc,17,' ','S'));
           PutTXT('    (----/S109) ABONO COMISIONES                     --   '||num2char(r.cuoabo_s109_accc,17,' ','S'));
           PutTXT('    (----/S110) CARGO COMISIONES                     --   '||num2char(r.cuocar_s110_accc,17,' ','S'));
           PutTXT('                                                          -----------------');
           PutTXT('    (----/S097) IMPORTE NETO                              '||num2char(r.imp_neto_signo,17,' ','S'));

           IF (vCntLotes+1) = vLotesXPag THEN
              vPagina:=vPagina+1;
              vCntLotes:=0;
              PutTXT(CHR(13));
           ELSE
              vCntLotes:=vCntLotes+1;
           END IF;

       END LOOP;
    END IF;

    COMMIT;
    IF pTipoOut = 'S' THEN
       UTL_FILE.FCLOSE(vIDFile);
       RETURN 0;
    ELSE
       RETURN vIDrpt;
    END IF;

EXCEPTION
    WHEN eFinError THEN
         ROLLBACK;
         IF pTipoOut = 'S' THEN
            UTL_FILE.FCLOSE(vIDFile);
         END IF;
         dbms_output.put_line('Paso: '||vpaso||' Msg: '||vErrMsg);
         RETURN -1;
    WHEN OTHERS THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         IF pTipoOut = 'S' THEN
            UTL_FILE.FCLOSE(vIDFile);
         END IF;
         dbms_output.put_line('Paso: '||vpaso||' oraErr: '||vOraErr);
         RETURN -1;
END; -- F_CLR_GENFILERPT


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- FUNCTION F_CLR_GENDATRPT_PCLM
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION F_CLR_GENDATRPT_PCLM (pCodEntAdq CHAR, pFecSesion DATE, pCodHrCierre CHAR) RETURN CHAR
IS

vTXNNumTxns CLR_RESUMEN_PCLM.TXN_NUMTXNS%TYPE:=0;
vTXNImpNeto CLR_RESUMEN_PCLM.TXN_IMPNETO%TYPE:=0;
vSGCNumTxns CLR_RESUMEN_PCLM.SGC_NUMTXNS%TYPE:=0;
vSGCImpNeto CLR_RESUMEN_PCLM.SGC_IMPNETO%TYPE:=0;
vFlgNeg     BOOLEAN;
vSigno      CHAR(1);
vMonto      NUMBER:=0;
vComiEMI    NUMBER:=0;
vMoneda     CHAR(3);
vOraCode    NUMBER;

BEGIN

  -- Elimina la Informacion Anterior
  DELETE CLR_RESUMEN_PCLM
   WHERE COD_ENTADQ = pCodEntAdq
     AND FEC_SESION = pFecSesion
     AND COD_HRCIERRE = pCodHrCierre;
  COMMIT;

  -- Codigo de Moneda
  vMoneda:=PQCOMERCIOS.GCW_F_GETMONEDAVIG(pFecSesion);


  -- Registra los Datos en la Tabla de Resumen
  INSERT INTO CLR_RESUMEN_PCLM (COD_ENTADQ,
                                FEC_SESION,
                                COD_HRCIERRE,
                                TXN_NUMTXNS,
                                TXN_IMPNETO,
                                SGC_NUMTXNS,
                                SGC_IMPNETO)
  SELECT COD_ENTADQ,FEC_SESION, COD_HRCIERRE,SUM(TXN_NUMTXNS),
         SUM(TXN_IMPNETO),SUM(SGC_NUMTXNS),SUM(SGC_IMPNETO)
  FROM CLR_RESUMEN_PCLM_HORA
     WHERE COD_ENTADQ = pCodEntAdq
     AND FEC_SESION = pFecSesion
     AND COD_HRCIERRE = pCodHrCierre
  GROUP BY COD_ENTADQ,FEC_SESION, COD_HRCIERRE ;

  COMMIT;
  
  SELECT TXN_NUMTXNS, TXN_IMPNETO,  SGC_NUMTXNS,  SGC_IMPNETO
  INTO vTXNNumTxns, vTXNImpNeto,  vSGCNumTxns, vSGCImpNeto
  FROM CLR_RESUMEN_PCLM
  WHERE COD_ENTADQ = pCodEntAdq
     AND FEC_SESION = pFecSesion
     AND COD_HRCIERRE = pCodHrCierre; 
  -- Cuadre
  IF vTXNNumTxns <> vSGCNumTxns OR vTXNImpNeto <> vSGCImpNeto THEN
     --- Colocado por TST de forma temporal 01/06/2018 
     --- Se debe eliminar 
    /* INSERT INTO SGCVNZ.CLR_RESUMEN_PCLM_BORRAR (COD_ENTADQ,FEC_SESION,COD_HRCIERRE,TXN_NUMTXNS,TXN_IMPNETO,SGC_NUMTXNS,SGC_IMPNETO,RETURN_VALUE)
                                                                       VALUES (pCodEntAdq,pFecSesion,pCodHrCierre,vTXNNumTxns,vTXNImpNeto,vSGCNumTxns, vSGCImpNeto,'1');
    COMMIT;                                                                       
    --- Fin colocado por TST temporal 01/06/2018
    */
     RETURN '1';
  ELSE
      --- Colocado por TST de forma temporal 01/06/2018 
     --- Se debe eliminar 
     /*SERT INTO SGCVNZ.CLR_RESUMEN_PCLM_BORRAR (COD_ENTADQ,FEC_SESION,COD_HRCIERRE,TXN_NUMTXNS,TXN_IMPNETO,SGC_NUMTXNS,SGC_IMPNETO,RETURN_VALUE)
                                                                       VALUES (pCodEntAdq,pFecSesion,pCodHrCierre,vTXNNumTxns,vTXNImpNeto,vSGCNumTxns, vSGCImpNeto,'0');
      COMMIT;                                                                       
    --- Fin colocado por TST temporal 01/06/2018       
    */                                                        
     RETURN '0';
  END IF;

EXCEPTION
  WHEN OTHERS THEN
       vOraCode:=abs(SQLCODE);
       ROLLBACK;
       RETURN 'E|Error de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')';
END;

/*
PROCEDURE P_COMP_GENDATRPT0304 (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1))
IS

vS105 NUMBER(16) := 0;
vS106 NUMBER(16) := 0;
vS107 NUMBER := 0;
vS108 NUMBER := 0;

vimp_neto_comp NUMBER(16) := 0;
vimp_neto_signo_comp NUMBER(16) := 0;

vS86 NUMBER(16) := 0;
vS87 NUMBER(16) := 0;
vS88 NUMBER(16) := 0;
vS89 NUMBER(16) := 0;
vS109 NUMBER(16) := 0;
vS110 NUMBER(16) := 0;
vS113 NUMBER(16) := 0;
vS114 NUMBER(16) := 0;
vS115 NUMBER(16) := 0;
vS116 NUMBER(16) := 0;

vS117_1 NUMBER(16) := 0;
vS117_2 NUMBER(16) := 0;
vS117 NUMBER(16) := 0;
vS118 NUMBER(16) := 0;
vS119 NUMBER(16) := 0;


vAux        VARCHAR2(4);
vCod_entidadM NUMBER;
vTipoSaldo  CHAR(1);
vOraCode    NUMBER;
vIdProc     NUMBER;
vRetC       VARCHAR2(100);
vOraErr     VARCHAR2(200);
vErrMsg     VARCHAR2(200);
vPaso       VARCHAR2(3);
eFinError   EXCEPTION;


BEGIN

vIDProc := PQMONPROC.InsMonProc ('PGENRPT0304');


-- Reversion: No hay

-- Procesa retornos MARCAS
vPaso := '010';

FOR rCont IN (SELECT count(*) s115, SUM(rr.imptra_p04_actc) s116 ,
             SUM(DECODE(sigcu1_p46_actc,'C',impcu1_p46_actc,-1*impcu1_p46_actc)) S119_1,
             SUM(DECODE(sigcu2_p46_actc,'C',impcu2_p46_actc,-1*impcu2_p46_actc)) S119_2,
             rr.inlote_p29_actc num_lote, rr.tipmov_p48_actc tipmov,
             TO_NUMBER(SUBSTR(rr.ideaut_p58_actc,3,4)) cod_entidad,
             rr.montra_p49_actc cod_moneda,
             SUM(f_getcomisionMC(rr.PDS0158_P48, rr.SESION_P28_ACTC, rr.IMPTRA_P04_ACTC,
               rr.TIPTRA_P48_ACTC, rr.CODACT_P18_ACTC, rr.TIPMOV_P48_ACTC )) S119_3
             FROM SGDVNZ.RECHAZOS_RETORNOS rr, COMERCIOS_PMP cp
             WHERE
                cp.cod_horacierre      = pCodHrCierre
                AND rr.COD_ENTADQ      = pEntAdq
                AND rr.FEC_SESION      = pFecSesion
                AND rtrim(rr.ideest_p42_actc) = cp.cod_comercio
                AND TO_NUMBER(SUBSTR(rr.ideaut_p58_actc,3,4)) in (105,108)
                AND rr.inlote_p29_actc in ('512','514','516')
              group by
                rr.inlote_p29_actc, rr.tipmov_p48_actc , TO_NUMBER(SUBSTR(rr.ideaut_p58_actc,3,4)), rr.montra_p49_actc, rr.PDS0158_P48, rr.tiptra_p48_actc

              ) LOOP

    BEGIN


     dbms_output.put_line('Procesa Retornos : '||to_char(rCont.s115) || 'Importe : '||to_char(rCont.s116));

     -- Calcula comisiones emisor
        vS119 := rCont.S119_1 + rCont.S119_2 + rCont.S119_3;

     vPaso := '020';
     SELECT
        NVL(IMPABO_S86_ACCC,0),
        NVL(IMPANA_S87_ACCC,0),
        NVL(IMPCAR_S88_ACCC,0),
        NVL(IMPANC_S89_ACCC,0),
        NVL(ICHAAB_S105_ACCC,0),
        NVL(ICHACA_S106_ACCC,0),
        NVL(CUOABO_S109_ACCC,0),
        NVL(CUOCAR_S110_ACCC,0),
        NVL(ABOMIS_S113_ACCC,0),
        NVL(CARMIS_S114_ACCC,0),
        NVL(NRECHAZOS_S115_ACCC,0),
        NVL(RECHAZOS_S116_ACCC,0),
        NVL(CUOCHAAB_S117_ACCC,0),
        NVL(CUOCHACA_S118_ACCC,0)
      INTO
        vs86,
        vs87,
        vs88,
        vs89,
        vs105,
        vs106,
        vs109,
        vs110,
        vs113,
        vs114,
        vs115,
        vS116,
        vS117,
        vS118
      FROM CLR_RESUMEN
      WHERE
      cod_hrcierre    = pCodHrCierre
      AND cod_entadq  = pEntAdq
      AND fec_sesion  = pFecSesion
      AND num_lote    = rCont.num_lote
      AND tipmov      = rCont.tipmov
      AND cod_entidad = rCont.cod_entidad
      AND cod_moneda  = rCont.cod_moneda
      AND tipo_lote = 'E';

     vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - vS105 + vs106  + vS109 - vS110 + vS113 - vS114 - rCont.S116 + vS117 - vS118 + vS119;


     IF vimp_neto_signo_comp >= 0 THEN
        vTipoSaldo := 'C';
     ELSE
        vTipoSaldo := 'D';
     END IF;
    dbms_output.put_line('Actualiza CLR_Resumen : '||to_char(rCont.num_lote) || 'Importe : '||to_char(rCont.s116));
    vPaso := '030';
    UPDATE CLR_RESUMEN SET
      NRECHAZOS_S115_ACCC = rCont.S115,
      RECHAZOS_S116_ACCC  = rCont.S116,
      CUORECHAZOS_S119_ACCC = vS119,
      IMP_NETO_COMP            = ABS(vimp_neto_signo_comp),
      IMP_NETO_SIGNO_COMP       = vimp_neto_signo_comp,
      TIPO_SALDO_COMP          = vTipoSaldo
    WHERE
      cod_hrcierre    = pCodHrCierre
      AND cod_entadq  = pEntAdq
      AND fec_sesion  = pFecSesion
      AND num_lote    = rCont.num_lote
      AND tipmov      = rCont.tipmov
      AND cod_entidad = rCont.cod_entidad
      AND cod_moneda  = rCont.cod_moneda
      AND tipo_lote = 'E';


    EXCEPTION

      WHEN no_data_found THEN
         dbms_output.put_line('Inserta CLR_Resumen : '||to_char(rCont.num_lote) || 'Importe : '||to_char(rCont.s116));
         INSERT INTO CLR_RESUMEN(
            FEC_SESION,
            COD_HRCIERRE,
            COD_MONEDA,
            TIPO_LOTE,
            COD_ENTIDAD,
            NUM_LOTE,
            TIPO_SALDO,
            NUM_OPER,
            IMP_NETO,
            IMP_NETO_SIGNO,
            TIPO_SALDO_COMP,
            NUM_OPER_COMP,
            IMP_NETO_COMP,
            IMP_NETO_SIGNO_COMP,
            NUMABO_S74_ACCC,
            NUMANA_S75_ACCC,
            NUMCAR_S76_ACCC,
            NUMANC_S77_ACCC,
            IMPABO_S86_ACCC,
            IMPANA_S87_ACCC,
            IMPCAR_S88_ACCC,
            IMPANC_S89_ACCC,
            ICHAAB_S105_ACCC,
            ICHACA_S106_ACCC,
            NCHAAB_S107_ACCC,
            NCHACA_S108_ACCC,
            CUOABO_S109_ACCC,
            CUOCAR_S110_ACCC,
            NABOMIS_S111_ACCC,
            NCARMIS_S112_ACCC,
            ABOMIS_S113_ACCC,
            CARMIS_S114_ACCC,
            COD_ENTADQ,
            TIPMOV,
            NRECHAZOS_S115_ACCC,
            RECHAZOS_S116_ACCC,
            CUOCHAAB_S117_ACCC,
            CUOCHACA_S118_ACCC,
            CUORECHAZOS_S119_ACCC )
       VALUES
           (pFecSesion, pCodHrCierre, rCont.cod_moneda, 'E',
            rCont.cod_entidad, rCont.num_lote,
            'C', 0, 0,0,
            'D', rCont.s115, ABS (rCont.s116), -1*rCont.s116,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            pEntAdq, rCont.tipmov ,rCont.s115, rCont.s116,0,0,vs119);

            dbms_output.put_line('CLR_Resumen insertado: '||to_char(rCont.num_lote) || 'Importe : '||to_char(rCont.s116));
      WHEN others THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         --RAISE_APPLICATION_ERROR(-20000,vOraErr);
      END;
   END LOOP;


-- Procesa reversos de contracargos
   FOR rCont in  (
      SELECT COUNT(*) s107,
         SUM(dm.incoming_dest_amount) s105,
         doo.p29inlote num_lote,
         doo.p48tipmov tipmov,
         doo.cod_bcoemi cod_entidad ,
         cp.cod_horacierre cod_hrcierre,
         dm.incoming_dest_curren_code cod_moneda
         --INTO vS105 ,  vS107 , vtipmov, vcod_entidad, vcod_hrcierre, vcod_moneda
       FROM SGDVNZ.DISPUTAS_MASTER dm, SGDVNZ.DISPUTAS_OPERACION_ORIGINAL doo,
         comercios_pmp cp
       WHERE dm.Disputa_type IN (35,36)
         --AND dm.gestor_action_code = 'XX'
         AND dm.cod_entadq = pEntAdq
         AND dm.incoming_date = pFecSesion
         AND dm.movdis_id = doo.movdis_id
         AND doo.cod_comercio = cp.cod_comercio
       GROUP BY
         doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code
        ) LOOP


     -- genera registro de Emisor  para reversiones de contracargos
     BEGIN

     SELECT
        NVL(IMPABO_S86_ACCC,0),
        NVL(IMPANA_S87_ACCC,0),
        NVL(IMPCAR_S88_ACCC,0),
        NVL(IMPANC_S89_ACCC,0),
        NVL(ICHAAB_S105_ACCC,0),
        NVL(ICHACA_S106_ACCC,0),
        NVL(CUOABO_S109_ACCC,0),
        NVL(CUOCAR_S110_ACCC,0),
        NVL(ABOMIS_S113_ACCC,0),
        NVL(CARMIS_S114_ACCC,0),
        NVL(NRECHAZOS_S115_ACCC,0),
        NVL(RECHAZOS_S116_ACCC,0)
      INTO
        vs86,
        vs87,
        vs88,
        vs89,
        vs105,
        vs106,
        vs109,
        vs110,
        vs113,
        vs114,
        vs115,
        vS116
      FROM CLR_RESUMEN
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'E' AND
        num_lote =  DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516) AND
        tipmov = rCont.tipmov AND
        cod_entidad = rCont.cod_entidad AND
        fec_sesion = pFecSesion ;

     vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - rCont.S105 + vs106  + vS109 - vS110 + vS113 - vS114 - vS116;


     IF vimp_neto_signo_comp >= 0 THEN
        vTipoSaldo := 'C';
     ELSE
        vTipoSaldo := 'D';
     END IF;

     UPDATE CLR_RESUMEN
      SET ICHAAB_S105_ACCC =  rCont.s105,
        NCHAAB_S107_ACCC =  rCont.s107,
        IMP_NETO_COMP  = ABS(vimp_neto_signo_comp),
        IMP_NETO_SIGNO_COMP = vimp_neto_signo_comp,
        tipo_saldo_comp = vTipoSaldo
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'E' AND
        num_lote =  DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516) AND
        --num_lote =  rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = rCont.cod_entidad AND
        fec_sesion = pFecSesion ;

    EXCEPTION

      WHEN no_data_found THEN

          INSERT INTO CLR_RESUMEN (
            FEC_SESION,
            COD_HRCIERRE,
            COD_MONEDA,
            TIPO_LOTE,
            COD_ENTIDAD,
            NUM_LOTE,
            TIPO_SALDO,
            NUM_OPER,
            IMP_NETO,
            IMP_NETO_SIGNO,
            TIPO_SALDO_COMP,
            NUM_OPER_COMP,
            IMP_NETO_COMP,
            IMP_NETO_SIGNO_COMP,
            NUMABO_S74_ACCC,
            NUMANA_S75_ACCC,
            NUMCAR_S76_ACCC,
            NUMANC_S77_ACCC,
            IMPABO_S86_ACCC,
            IMPANA_S87_ACCC,
            IMPCAR_S88_ACCC,
            IMPANC_S89_ACCC,
            ICHAAB_S105_ACCC,
            ICHACA_S106_ACCC,
            NCHAAB_S107_ACCC,
            NCHACA_S108_ACCC,
            CUOABO_S109_ACCC,
            CUOCAR_S110_ACCC,
            NABOMIS_S111_ACCC,
            NCARMIS_S112_ACCC,
            ABOMIS_S113_ACCC,
            CARMIS_S114_ACCC,
            COD_ENTADQ,
            TIPMOV,
            NRECHAZOS_S115_ACCC,
            RECHAZOS_S116_ACCC )
       VALUES
           (pFecSesion, rCont.cod_hrcierre, rCont.cod_moneda, 'E',
            rCont.cod_entidad,  DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516),
            'C', 0, 0, 0,
            'D', rCont.s107, ABS (rCont.s105), -1*rCont.s105,
            0,0,0,0,0,0,0,0, rCont.s105 ,0, rCont.s107, 0,0,0,0,0,0,0,
            pEntAdq, rCont.tipmov, 0,0);

      WHEN others THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);
      END;
   END LOOP;



   -- genera registro de merchant para reversiones de contracargos
   FOR rCont in  (

        SELECT COUNT(*) s107,
         SUM(dm.incoming_dest_amount) s105,
         doo.p29inlote num_lote,
         doo.p48tipmov tipmov,
         doo.cod_bcoemi cod_entidad ,
         cp.cod_horacierre cod_hrcierre,
         dm.incoming_dest_curren_code cod_moneda,
         substr(doo.p48tiptra,1,2) p48tiptra
         --INTO vS105 ,  vS107 , vtipmov, vcod_entidad, vcod_hrcierre, vcod_moneda
       FROM SGDVNZ.DISPUTAS_MASTER dm, SGDVNZ.DISPUTAS_OPERACION_ORIGINAL doo,
         comercios_pmp cp
       WHERE dm.Disputa_type IN (35,36)
         --AND dm.gestor_action_code = 'XX'
         AND dm.cod_entadq = pEntAdq
         AND dm.incoming_date = pFecSesion
         AND dm.movdis_id = doo.movdis_id
         AND doo.cod_comercio = cp.cod_comercio
       GROUP BY
         doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code , substr(doo.p48tiptra,1,2)
        ) LOOP

     BEGIN

     vaux := '';
     IF pEntAdq = 'BM' THEN
        vaux := '98';
     ELSIF pEntAdq = 'BP' THEN
        vaux := '99';
     END IF;

     IF SUBSTR(rCont.p48tiptra,1,2) = '01' THEN
        vaux := '99'||vaux;
     ELSIF (SUBSTR(rCont.p48tiptra,1,2) = '06' OR SUBSTR(rCont.p48tiptra,1,2) = '10' )THEN
        vaux := '98'||vaux;
     ELSIF SUBSTR(rCont.p48tiptra,1,2) = '14' THEN
        vaux := '97'||vaux;
     ELSIF SUBSTR(rCont.p48tiptra,1,2) = '20' THEN
        vaux := '89'||vaux;
     END IF;

     vCod_entidadM := to_number(vaux) ;

     SELECT
        NVL(IMPABO_S86_ACCC,0),
        NVL(IMPANA_S87_ACCC,0),
        NVL(IMPCAR_S88_ACCC,0),
        NVL(IMPANC_S89_ACCC,0),
        NVL(ICHAAB_S105_ACCC,0),
        NVL(ICHACA_S106_ACCC,0),
        NVL(CUOABO_S109_ACCC,0),
        NVL(CUOCAR_S110_ACCC,0),
        NVL(ABOMIS_S113_ACCC,0),
        NVL(CARMIS_S114_ACCC,0),
        NVL(NRECHAZOS_S115_ACCC,0),
        NVL(RECHAZOS_S116_ACCC,0)
      INTO
        vs86,
        vs87,
        vs88,
        vs89,
        vs105,
        vs106,
        vs109,
        vs110,
        vs113,
        vs114,
        vs115,
        vS116
      FROM CLR_RESUMEN
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'M' AND
        num_lote =  rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = vCod_entidadM AND
        fec_sesion = pFecSesion ;

     vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - rCont.S105 + vs106  + vS109 - vS110 + vS113 - vS114 - vS116;

     IF vimp_neto_signo_comp >= 0 THEN
        vTipoSaldo := 'D';
     ELSE
        vTipoSaldo := 'C';
     END IF;
     vimp_neto_signo_comp := vimp_neto_signo_comp * -1 ;

     UPDATE CLR_RESUMEN
      SET ICHAAB_S105_ACCC =  rCont.s105,
        NCHAAB_S107_ACCC =  rCont.s107,
        IMP_NETO_COMP  = ABS(vimp_neto_signo_comp),
        IMP_NETO_SIGNO_COMP = vimp_neto_signo_comp,
        tipo_saldo_comp = vTipoSaldo
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'M' AND
        num_lote =  rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = vCod_entidadM AND
        fec_sesion = pFecSesion ;

    EXCEPTION

      WHEN no_data_found THEN

         INSERT INTO CLR_RESUMEN (
            FEC_SESION,
            COD_HRCIERRE,
            COD_MONEDA,
            TIPO_LOTE,
            COD_ENTIDAD,
            NUM_LOTE,
            TIPO_SALDO,
            NUM_OPER,
            IMP_NETO,
            IMP_NETO_SIGNO,
            TIPO_SALDO_COMP,
            NUM_OPER_COMP,
            IMP_NETO_COMP,
            IMP_NETO_SIGNO_COMP,
            NUMABO_S74_ACCC,
            NUMANA_S75_ACCC,
            NUMCAR_S76_ACCC,
            NUMANC_S77_ACCC,
            IMPABO_S86_ACCC,
            IMPANA_S87_ACCC,
            IMPCAR_S88_ACCC,
            IMPANC_S89_ACCC,
            ICHAAB_S105_ACCC,
            ICHACA_S106_ACCC,
            NCHAAB_S107_ACCC,
            NCHACA_S108_ACCC,
            CUOABO_S109_ACCC,
            CUOCAR_S110_ACCC,
            NABOMIS_S111_ACCC,
            NCARMIS_S112_ACCC,
            ABOMIS_S113_ACCC,
            CARMIS_S114_ACCC,
            COD_ENTADQ,
            TIPMOV,
            NRECHAZOS_S115_ACCC,
            RECHAZOS_S116_ACCC )
       VALUES
           (pFecSesion, rCont.cod_hrcierre, rCont.cod_moneda, 'M',
            vCod_entidadM, rCont.num_lote,
            'C', 0, 0, 0,
            'C', rCont.s107, ABS (rCont.s105), rCont.s105,
            0,0,0,0,0,0,0,0, rCont.s105 ,0, rCont.s107, 0,0,0,0,0,0,0,
            pEntAdq, rCont.tipmov, 0,0);

      WHEN others THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);
    END;

   END LOOP;


-- Procesa contracargos MARCAS


FOR rCont in  (
   SELECT COUNT(*) s108,
      SUM(dm.incoming_dest_amount) s106 ,
      SUM(f_getcomisionMC(INCOMING_PDS0158_IRD, doo.p28sesion, doo.p04imptra,
               doo.p48tiptra, doo.p18lacti, doo.p48tipmov )) S117_1,
      SUM(INCOMING_NATIONAL_REIMB_FEE) S117_2,
      doo.p29inlote num_lote,
      doo.p48tipmov tipmov,
      doo.cod_bcoemi cod_entidad ,
      cp.cod_horacierre cod_hrcierre,
      dm.incoming_dest_curren_code cod_moneda
   --INTO vS105 ,  vS107 , vtipmov, vcod_entidad, vcod_hrcierre, vcod_moneda
   FROM SGDVNZ.DISPUTAS_MASTER dm, SGDVNZ.DISPUTAS_OPERACION_ORIGINAL doo,
        comercios_pmp cp
   WHERE dm.Disputa_type IN (15,16,17)
      --AND dm.gestor_action_code = 'XX'
      AND dm.cod_entadq = pEntAdq
      AND dm.incoming_date = pFecSesion
      AND dm.movdis_id = doo.movdis_id
      AND doo.cod_comercio = cp.cod_comercio
      AND doo.cod_bcoemi in (8001,8010)
   GROUP BY
      doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code
      ) LOOP

    -- genera registro de Emisor  para contracargos
   BEGIN


    vPaso := '080';
    SELECT
        NVL(IMPABO_S86_ACCC,0),
        NVL(IMPANA_S87_ACCC,0),
        NVL(IMPCAR_S88_ACCC,0),
        NVL(IMPANC_S89_ACCC,0),
        NVL(ICHAAB_S105_ACCC,0),
        NVL(ICHACA_S106_ACCC,0),
        NVL(CUOABO_S109_ACCC,0),
        NVL(CUOCAR_S110_ACCC,0),
        NVL(ABOMIS_S113_ACCC,0),
        NVL(CARMIS_S114_ACCC,0),
        NVL(NRECHAZOS_S115_ACCC,0),
        NVL(RECHAZOS_S116_ACCC,0),
        NVL(CUOCHAAB_S117_ACCC,0),
        NVL(CUOCHACA_S118_ACCC,0),
        NVL(CUORECHAZOS_S119_ACCC,0)
      INTO
        vs86,
        vs87,
        vs88,
        vs89,
        vs105,
        vs106,
        vs109,
        vs110,
        vs113,
        vs114,
        vs115,
        vS116,
        vS117,
        vS118,
        vS119
      FROM CLR_RESUMEN
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'E' AND
        num_lote =  DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516) AND
        --num_lote = rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = rCont.cod_entidad AND
        fec_sesion = pFecSesion ;


     vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - vS105 + rCont.S106  + vS109 - vS110 + vS113 - vS114 + vS116 - (rCont.S117_1+rCont.S117_2) + vS118 - vS119;


     --dbms_output.put_line('S117 '||to_char(rCont.S117));

     IF vimp_neto_signo_comp >= 0 THEN
        vTipoSaldo := 'C';
     ELSE
        vTipoSaldo := 'D';
     END IF;

    vPaso := '090';


     UPDATE CLR_RESUMEN
      SET ICHACA_S106_ACCC =  rCont.s106,
        NCHACA_S108_ACCC =  rCont.s108,
        CUOCHAAB_S117_ACCC = rCont.s117_1+rCont.s117_2,
        CUOCHACA_S118_ACCC = 0, --rCont.s118,
        IMP_NETO_COMP  = ABS(vimp_neto_signo_comp),
        IMP_NETO_SIGNO_COMP = vimp_neto_signo_comp,
        tipo_saldo_comp = vTipoSaldo
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'E' AND
        num_lote =  DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516) AND
        --num_lote = rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = rCont.cod_entidad AND
        fec_sesion = pFecSesion ;


    EXCEPTION

      WHEN no_data_found THEN
          vPaso := '100';

         INSERT INTO CLR_RESUMEN (
            FEC_SESION,
            COD_HRCIERRE,
            COD_MONEDA,
            TIPO_LOTE,
            COD_ENTIDAD,
            NUM_LOTE,
            TIPO_SALDO,
            NUM_OPER,
            IMP_NETO,
            IMP_NETO_SIGNO,
            TIPO_SALDO_COMP,
            NUM_OPER_COMP,
            IMP_NETO_COMP,
            IMP_NETO_SIGNO_COMP,
            NUMABO_S74_ACCC,
            NUMANA_S75_ACCC,
            NUMCAR_S76_ACCC,
            NUMANC_S77_ACCC,
            IMPABO_S86_ACCC,
            IMPANA_S87_ACCC,
            IMPCAR_S88_ACCC,
            IMPANC_S89_ACCC,
            ICHAAB_S105_ACCC,
            ICHACA_S106_ACCC,
            NCHAAB_S107_ACCC,
            NCHACA_S108_ACCC,
            CUOABO_S109_ACCC,
            CUOCAR_S110_ACCC,
            NABOMIS_S111_ACCC,
            NCARMIS_S112_ACCC,
            ABOMIS_S113_ACCC,
            CARMIS_S114_ACCC,
            COD_ENTADQ,
            TIPMOV,
            NRECHAZOS_S115_ACCC,
            RECHAZOS_S116_ACCC ,
            CUOCHAAB_S117_ACCC,
            CUOCHACA_S118_ACCC,
            CUORECHAZOS_S119_ACCC)
       VALUES
           (pFecSesion, rCont.cod_hrcierre, rCont.cod_moneda, 'E',
            rCont.cod_entidad,DECODE(rCont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516),
            'C', 0, 0, 0,
            'C', rCont.s108, ABS (rCont.s106), rCont.s106,
            0,0,0,0,0,0,0,0, 0, rCont.s106, 0, rCont.s108 ,0,0,0,0,0,0,
            pEntAdq, rCont.tipmov, 0,0, rCont.s117_1+rCont.s117_2,0,0);
            --rCont.s118,0);


      WHEN others THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);
    END;
  END LOOP;



  FOR rCont in  (
   SELECT COUNT(*) s108,
      SUM(dm.incoming_dest_amount) s106 ,
      SUM(f_getcomisionMC(INCOMING_PDS0158_IRD, doo.p28sesion, doo.p04imptra,
               doo.p48tiptra, doo.p18lacti, doo.p48tipmov )) S117_1,
      SUM(INCOMING_NATIONAL_REIMB_FEE) S117_2,
      doo.p29inlote num_lote,
      doo.p48tipmov tipmov,
      doo.cod_bcoemi cod_entidad ,
      cp.cod_horacierre cod_hrcierre,
      dm.incoming_dest_curren_code cod_moneda,
      substr(doo.p48tiptra,1,2) p48tiptra
   --INTO vS105 ,  vS107 , vtipmov, vcod_entidad, vcod_hrcierre, vcod_moneda
   FROM SGDVNZ.DISPUTAS_MASTER dm, SGDVNZ.DISPUTAS_OPERACION_ORIGINAL doo,
        comercios_pmp cp
   WHERE dm.Disputa_type IN (15,16,17)
      --AND dm.gestor_action_code = 'XX'
      AND dm.cod_entadq = pEntAdq
      AND dm.incoming_date = pFecSesion
      AND dm.movdis_id = doo.movdis_id
      AND doo.cod_comercio = cp.cod_comercio
      AND doo.cod_bcoemi in (8001,8010)
   GROUP BY
      doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code , substr(doo.p48tiptra,1,2)
   ) LOOP

    -- genera registro de Merchant  para contracargos
   BEGIN

     vPaso := '110';
     vaux := '';
     IF pEntAdq = 'BM' THEN
        vaux := '98';
     ELSIF pEntAdq = 'BP' THEN
        vaux := '99';
     END IF;

     IF SUBSTR(rCont.p48tiptra,1,2) = '01' THEN
        vaux := '99'||vaux;
     ELSIF (SUBSTR(rCont.p48tiptra,1,2) = '06' OR SUBSTR(rCont.p48tiptra,1,2) = '10' )THEN
        vaux := '98'||vaux;
     ELSIF SUBSTR(rCont.p48tiptra,1,2) = '14' THEN
        vaux := '97'||vaux;
     ELSIF SUBSTR(rCont.p48tiptra,1,2) = '20' THEN
        vaux := '89'||vaux;
     END IF;

     vCod_entidadM := to_number(vaux) ;

     SELECT
        NVL(IMPABO_S86_ACCC,0),
        NVL(IMPANA_S87_ACCC,0),
        NVL(IMPCAR_S88_ACCC,0),
        NVL(IMPANC_S89_ACCC,0),
        NVL(ICHAAB_S105_ACCC,0),
        NVL(ICHACA_S106_ACCC,0),
        NVL(CUOABO_S109_ACCC,0),
        NVL(CUOCAR_S110_ACCC,0),
        NVL(ABOMIS_S113_ACCC,0),
        NVL(CARMIS_S114_ACCC,0),
        NVL(NRECHAZOS_S115_ACCC,0),
        NVL(RECHAZOS_S116_ACCC,0),
        NVL(CUOCHAAB_S117_ACCC,0),
        NVL(CUOCHACA_S118_ACCC,0),
        NVL(CUORECHAZOS_S119_ACCC,0)
      INTO
        vs86,
        vs87,
        vs88,
        vs89,
        vs105,
        vs106,
        vs109,
        vs110,
        vs113,
        vs114,
        vs115,
        vS116,
        vS117,
        vS118,
        vS119
      FROM CLR_RESUMEN
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'M' AND
        num_lote =  rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = vCod_entidadM AND
        fec_sesion = pFecSesion ;

     vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - vS105 + rCont.S106  + vS109 - vS110 + vS113 - vS114 + vS116 - (rCont.S117_1 + rCont.S117_2) +VS118 -VS119;
       --+ rCont.S118 - vS119;

     IF vimp_neto_signo_comp >= 0 THEN
        vTipoSaldo := 'D';
     ELSE
        vTipoSaldo := 'C';
     END IF;
     vimp_neto_signo_comp := vimp_neto_signo_comp * -1 ;

     vPaso := '120';

     UPDATE CLR_RESUMEN
      SET ICHACA_S106_ACCC =  rCont.s106,
        NCHACA_S108_ACCC =  rCont.s108,
        CUOCHAAB_S117_ACCC = rCont.s117_1 + rCont.s117_2,
        IMP_NETO_COMP  = ABS(vimp_neto_signo_comp),
        IMP_NETO_SIGNO_COMP = vimp_neto_signo_comp,
        tipo_saldo_comp = vTipoSaldo
      WHERE cod_entadq = pEntAdq AND
        cod_hrcierre = rCont.cod_hrcierre AND
        cod_moneda = rCont.cod_moneda AND
        tipo_lote = 'M' AND
        num_lote =  rCont.num_lote AND
        tipmov = rCont.tipmov AND
        cod_entidad = vCod_entidadM AND
        fec_sesion = pFecSesion ;


    EXCEPTION

      WHEN no_data_found THEN

         INSERT INTO CLR_RESUMEN (
            FEC_SESION,
            COD_HRCIERRE,
            COD_MONEDA,
            TIPO_LOTE,
            COD_ENTIDAD,
            NUM_LOTE,
            TIPO_SALDO,
            NUM_OPER,
            IMP_NETO,
            IMP_NETO_SIGNO,
            TIPO_SALDO_COMP,
            NUM_OPER_COMP,
            IMP_NETO_COMP,
            IMP_NETO_SIGNO_COMP,
            NUMABO_S74_ACCC,
            NUMANA_S75_ACCC,
            NUMCAR_S76_ACCC,
            NUMANC_S77_ACCC,
            IMPABO_S86_ACCC,
            IMPANA_S87_ACCC,
            IMPCAR_S88_ACCC,
            IMPANC_S89_ACCC,
            ICHAAB_S105_ACCC,
            ICHACA_S106_ACCC,
            NCHAAB_S107_ACCC,
            NCHACA_S108_ACCC,
            CUOABO_S109_ACCC,
            CUOCAR_S110_ACCC,
            NABOMIS_S111_ACCC,
            NCARMIS_S112_ACCC,
            ABOMIS_S113_ACCC,
            CARMIS_S114_ACCC,
            COD_ENTADQ,
            TIPMOV,
            NRECHAZOS_S115_ACCC,
            RECHAZOS_S116_ACCC ,
            CUOCHAAB_S117_ACCC,
            CUOCHACA_S118_ACCC,
            CUORECHAZOS_S119_ACCC)
       VALUES
           (pFecSesion, rCont.cod_hrcierre, rCont.cod_moneda, 'M',
            vCod_entidadM, rCont.num_lote ,
            'C', 0, 0, 0,
            'D', rCont.s108, ABS (rCont.s106), -1*rCont.s106,
            0,0,0,0,0,0,0,0, 0, rCont.s106, 0, rCont.s108 ,0,0,0,0,0,0,
            pEntAdq, rCont.tipmov, 0,0, rCont.s117_1 + rCont.s117_2,0,0);


      WHEN others THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);
    END;

END LOOP;


COMMIT;
PQMONPROC.InsLog(vIDProc, 'F', 'FIN OK | Fecha de Sesion: '||TO_CHAR(pFecSesion,'YYYY/MM/DD'));
vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'M', '0');



EXCEPTION
    WHEN OTHERS THEN
         vOraErr:=SUBSTR(SQLERRM,1,200);
         ROLLBACK;
         PQMONPROC.InsLog(vIDProc, 'E', vOraErr);
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);

END;
*/

FUNCTION F_COMP_GENFILERPT0204 (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion date, pTipoFile CHAR) RETURN CHAR
IS
    vIdProc              pls_integer      := 0;
    vIDFile              utl_file.file_type;
    vdirout              varchar2(100)    := 'DIR-OUT';
    vFile                varchar2(50)     := '';
    vOraCode             number           := 0;
    vErrMsg              varchar2(200)    := '';
    vOraErr              varchar2(200)    := '';
    vRetC                varchar2(100)    := '';
    vPaso                varchar2(10)     := '';
    vErrCod              varchar2(2)      := '';
    eFinError            exception;
    vb_isdate            boolean;
    gdfecha              date;
    vsFecha              varchar2(8)      := '';
    vstr                 varchar2(999)    := '';
    vTipMov              char(1)          := '';
    vNumRegEnviados      number           := 0;
    vSumImpEnviado       number(18,2)     := 0; ---- Estaba como number(16,2)  08/04/2018 TST
    vNumRegEnviados_aux  char(10)         := '';
    vSumImpEnviado_aux   char(17)         := ''; --- Estaba como char(17)   08/04/2018 TST
    vRegEncoDetalle      char(10)         := '';
    vImpNetFindDetalle   char(17)         := '';  --- Estaba como char(17)  08/04/2018 TST
    verrorCab            char(3)          := '';
    vfillerCab           varchar2(573)    := '';
    vHrCierre            char(1)          := '';
    vCsb                 char(4)          := '';
    vS097                number(16,2)     := 0;
    vC097                CHAR(17)         :='';
    vflg_tipmovc         CHAR(1)          :='0';
    vflg_tipmovd         CHAR(1)          :='0';
    vEntadq_aux          CHAR(4);



cursor cur_detalle_02 is             ----------CLCO02 --------------
  select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
           decode(sum(sign(imp_neto_signo)),-1,
                   'C'||lpad(sum(imp_neto),16,0),
                   'D'||lpad(sum(imp_neto),16,0))  S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(0,16,0)       S105, --D
         lpad(0,16,0)       S106, --D
         lpad(0,10,0)       S107, --D
         lpad(0,10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(0,10,0)    S115, --D
         lpad(0,16,0)     S116,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen
   where fec_sesion    = pFecSesion
     and cod_hrcierre  = pCodHrCierre
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
     and num_lote not in (611, 614)
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   order by cod_entidad, num_lote ;


cursor cur_detalle_04 is             ----------CLCO04--------------
  select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
/*           decode(sum(sign(imp_neto_signo_comp)),-1,
                   'C'||lpad(sum(imp_neto_comp),16,0),
                   'D'||lpad(sum(imp_neto_comp),16,0))      S097, --DN*/
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)  S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D

         lpad(sum(nvl(RECHCAR_S_S123_ACCC,0)),16,0)     S123,  --D
         lpad(sum(nvl(NRECHCAR_S_S124_ACCC,0)),10,0)     S124,  --D
         lpad(sum(nvl(RECHAZOS_S_S125_ACCC,0)),16,0)     S125,  --D
         lpad(sum(nvl(NRECHAZOS_S_S126_ACCC,0)),10,0)     S126,  --D
         lpad(sum(nvl(CUORECHAZOS_S_S127_ACCC,0)),16,0)     S127,  --D
         lpad(sum(nvl(CUORECHABO_S_S128_ACCC,0)),16,0)     S128,  --D
         lpad(sum(nvl(NREPABO_S129_ACCC,0)),10,0)     S129,  --D
         lpad(sum(nvl(REPABO_S130_ACCC,0)),16,0)     S130,  --D
         lpad(sum(nvl(NREPCAR_S131_ACCC,0)),10,0)     S131,  --D
         lpad(sum(nvl(REPCAR_S132_ACCC,0)),16,0)     S132,  --D


         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_platco
   where fec_sesion    = pFecSesion
     and (cod_entadq    = pEntAdq OR cod_entidad||cod_entadq = '108BM' OR cod_entidad||cod_entadq = '105BP')
     and tipo_lote     = 'E'
     and cod_entidad||cod_entadq NOT in ('108BP','105BM')
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   UNION
  select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
         /*  decode(sum(sign(imp_neto_signo_comp)),-1,
                   'C'||lpad(sum(imp_neto_comp),16,0),
                   'D'||lpad(sum(imp_neto_comp),16,0))      S097, --DN*/
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)     S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(sum(nvl(RECHCAR_S_S123_ACCC,0)),16,0)     S123,  --D
         lpad(sum(nvl(NRECHCAR_S_S124_ACCC,0)),10,0)     S124,  --D
         lpad(sum(nvl(RECHAZOS_S_S125_ACCC,0)),16,0)     S125,  --D
         lpad(sum(nvl(NRECHAZOS_S_S126_ACCC,0)),10,0)     S126,  --D
         lpad(sum(nvl(CUORECHAZOS_S_S127_ACCC,0)),16,0)     S127,  --D
         lpad(sum(nvl(CUORECHABO_S_S128_ACCC,0)),16,0)     S128,  --D
         lpad(sum(nvl(NREPABO_S129_ACCC,0)),10,0)     S129,  --D
         lpad(sum(nvl(REPABO_S130_ACCC,0)),16,0)     S130,  --D
         lpad(sum(nvl(NREPCAR_S131_ACCC,0)),10,0)     S131,  --D
         lpad(sum(nvl(REPCAR_S132_ACCC,0)),16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_mc
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   UNION
     select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
         /*  decode(sum(sign(imp_neto_signo_comp)),-1,
                   'C'||lpad(sum(imp_neto_comp),16,0),
                   'D'||lpad(sum(imp_neto_comp),16,0))      S097, --DN*/
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)  S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(sum(nvl(RECHCAR_S_S123_ACCC,0)),16,0)     S123,  --D
         lpad(sum(nvl(NRECHCAR_S_S124_ACCC,0)),10,0)     S124,  --D
         lpad(sum(nvl(RECHAZOS_S_S125_ACCC,0)),16,0)     S125,  --D
         lpad(sum(nvl(NRECHAZOS_S_S126_ACCC,0)),10,0)     S126,  --D
         lpad(sum(nvl(CUORECHAZOS_S_S127_ACCC,0)),16,0)     S127,  --D
         lpad(sum(nvl(CUORECHABO_S_S128_ACCC,0)),16,0)     S128,  --D
         lpad(sum(nvl(NREPABO_S129_ACCC,0)),10,0)     S129,  --D
         lpad(sum(nvl(REPABO_S130_ACCC,0)),16,0)     S130,  --D
         lpad(sum(nvl(NREPCAR_S131_ACCC,0)),10,0)     S131,  --D
         lpad(sum(nvl(REPCAR_S132_ACCC,0)),16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_visa
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   UNION
  select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
         /*  decode(sum(sign(imp_neto_signo_comp)),-1,
                   'C'||lpad(sum(imp_neto_comp),16,0),
                   'D'||lpad(sum(imp_neto_comp),16,0))      S097, --DN*/
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)  S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(0,16,0)     S123,  --D
         lpad(0,10,0)     S124,  --D
         lpad(0,16,0)     S125,  --D
         lpad(0,10,0)     S126,  --D
         lpad(0,16,0)     S127,  --D
         lpad(0,16,0)     S128,  --D
         lpad(0,10,0)     S129,  --D
         lpad(0,16,0)     S130,  --D
         lpad(0,10,0)     S131,  --D
         lpad(0,16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_maestro
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
     and num_lote <> '816'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
  -- order by cod_entidad, num_lote 
   UNION
	select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)     S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(sum(nvl(RECHCAR_S_S123_ACCC,0)),16,0)     S123,  --D
         lpad(sum(nvl(NRECHCAR_S_S124_ACCC,0)),10,0)     S124,  --D
         lpad(sum(nvl(RECHAZOS_S_S125_ACCC,0)),16,0)     S125,  --D
         lpad(sum(nvl(NRECHAZOS_S_S126_ACCC,0)),10,0)     S126,  --D
         lpad(sum(nvl(CUORECHAZOS_S_S127_ACCC,0)),16,0)     S127,  --D
         lpad(sum(nvl(CUORECHABO_S_S128_ACCC,0)),16,0)     S128,  --D
         lpad(sum(nvl(NREPABO_S129_ACCC,0)),10,0)     S129,  --D
         lpad(sum(nvl(REPABO_S130_ACCC,0)),16,0)     S130,  --D
         lpad(sum(nvl(NREPCAR_S131_ACCC,0)),10,0)     S131,  --D
         lpad(sum(nvl(REPCAR_S132_ACCC,0)),16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_mc_ngta
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   UNION
    select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
         decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)  S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(sum(nvl(RECHCAR_S_S123_ACCC,0)),16,0)     S123,  --D
         lpad(sum(nvl(NRECHCAR_S_S124_ACCC,0)),10,0)     S124,  --D
         lpad(sum(nvl(RECHAZOS_S_S125_ACCC,0)),16,0)     S125,  --D
         lpad(sum(nvl(NRECHAZOS_S_S126_ACCC,0)),10,0)     S126,  --D
         lpad(sum(nvl(CUORECHAZOS_S_S127_ACCC,0)),16,0)     S127,  --D
         lpad(sum(nvl(CUORECHABO_S_S128_ACCC,0)),16,0)     S128,  --D
         lpad(sum(nvl(NREPABO_S129_ACCC,0)),10,0)     S129,  --D
         lpad(sum(nvl(REPABO_S130_ACCC,0)),16,0)     S130,  --D
         lpad(sum(nvl(NREPCAR_S131_ACCC,0)),10,0)     S131,  --D
         lpad(sum(nvl(REPCAR_S132_ACCC,0)),16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_visa_ngta
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   UNION
	select lpad(sum(nvl(numabo_s74_accc,0)),10,0)        S074, --D
         lpad(sum(nvl(numana_s75_accc,0)),10,0)        S075, --D
         lpad(sum(nvl(numcar_s76_accc,0)),10,0)        S076, --D
         lpad(sum(nvl(numanc_s77_accc,0)),10,0)        S077, --D
         lpad(sum(nvl(impabo_s86_accc,0)),16,0)        S086, --D
         lpad(sum(nvl(impana_s87_accc,0)),16,0)        S087, --D
         lpad(sum(nvl(impcar_s88_accc,0)),16,0)        S088, --D
         lpad(sum(nvl(impanc_s89_accc,0)),16,0)        S089, --D
           decode(sum(sign(imp_neto_signo_comp)),1,
                   'D'||lpad(sum(imp_neto_comp),16,0),
                   'C'||lpad(sum(imp_neto_comp),16,0))      S097, --DN
         sum(imp_neto_signo_comp)                      S097_aux,
         lpad(sum(nvl(ichaab_s105_accc,0)),16,0)       S105, --D
         lpad(sum(nvl(ichaca_s106_accc,0)),16,0)       S106, --D
         lpad(sum(nvl(nchaab_s107_accc,0)),10,0)       S107, --D
         lpad(sum(nvl(nchaca_s108_accc,0)),10,0)       S108, --D
         lpad(sum(nvl(cuoabo_s109_accc,0)),16,0)       S109, --D
         lpad(sum(nvl(cuocar_s110_accc,0)),16,0)       S110, --D
         lpad(sum(nvl(nabomis_s111_accc,0)),10,0)      S111, --D
         lpad(sum(nvl(ncarmis_s112_accc,0)),10,0)      S112, --D
         lpad(sum(nvl(abomis_s113_accc,0)),16,0)       S113, --D
         lpad(sum(nvl(carmis_s114_accc,0)),16,0)       S114, --D
         lpad(sum(nvl(nrechazos_s115_accc,0)),10,0)    S115, --D
         lpad(sum(nvl(rechazos_s116_accc,0)),16,0)     S116,  --D
         lpad(sum(nvl(CUOCHAAB_S117_ACCC,0)),16,0)     S117,  --D
         lpad(sum(nvl(CUOCHACA_S118_ACCC,0)),16,0)     S118,  --D
         lpad(sum(nvl(CUORECHAZOS_S119_ACCC,0)),16,0)  S119,  --D
         lpad(sum(nvl(RECHCAR_S120_ACCC,0)),16,0)      S120,  --D
         lpad(sum(nvl(CUORECHABO_S121_ACCC,0)),16,0)   S121,  --D
         lpad(sum(nvl(NRECHCAR_S122_ACCC,0)),10,0)     S122,  --D
         lpad(0,16,0)     S123,  --D
         lpad(0,10,0)     S124,  --D
         lpad(0,16,0)     S125,  --D
         lpad(0,10,0)     S126,  --D
         lpad(0,16,0)     S127,  --D
         lpad(0,16,0)     S128,  --D
         lpad(0,10,0)     S129,  --D
         lpad(0,16,0)     S130,  --D
         lpad(0,10,0)     S131,  --D
         lpad(0,16,0)     S132,  --D
         lpad(cod_entadq,2,'0') cod_entadq,
         lpad(cod_entidad,4,'0') cod_entidad,
         rpad(num_lote,3,' ') num_lote , tipmov, cod_moneda
    from clr_resumen_maestro_ngta
   where fec_sesion    = pFecSesion
     and cod_entadq    = pEntAdq
     and tipo_lote     = 'E'
     and num_lote <> '816'
   group by cod_entadq, cod_entidad, num_lote, tipmov, cod_moneda
   order by cod_entidad, num_lote;

procedure PutTXT(pTexto CHAR) is
begin
  UTL_FILE.PUT(vIDFile,pTexto);UTL_FILE.New_Line(vIDFile);
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


BEGIN

    --validacion de directorio
    vPaso      := 'Paso 01';
    vIDProc    := pqmonproc.InsMonProc ('PRPT0204');
    vDirOut     := STD.F_GETVALPAR('DIR-OUT');

    if vDirOut is null then
       verrmsg:='fin error | directorio de salida (dir_out) no definido en std_parametro.';
       verrcod := '2';
       raise efinerror;
    end if;

    --Validando si la fecha del archivo es ok
    vPaso       := 'Paso 02';
    vsfecha     := to_char(pFecSesion,'yyyymmdd');
    vb_isdate   := f_IsDate(vsfecha);

    if vb_isDate = false then
       verrmsg  := 'Error, fecha de archivo incorrecto[YYYYMMDD]';
       verrcod  := '3';
      raise efinerror;
    end  if;

    --Validando nro de Cierre
    if pTipoFile!='04' then
        if pCodHrCierre not in('1','2')then
           verrmsg:='error|Hora de Cierre Incorrecta [1=21:00 Horas/2=02:00 Horas]';
           verrcod := '4';
           raise efinerror;
        end if;
    end if;

    --valida si Banco ok
    vpaso   := 'Paso 02a';
    if pEntAdq = 'BM' then
       vCsb := '0105';
    else
       if pEntAdq = 'BP' then
          vCsb := '0108';
       else
          verrmsg := 'error Adquirente incorrecto [BM=Banco Mercantil/BP=Banco Provincial]';
          verrcod := '99';
          raise efinerror;
       end if ;
    end if;

    if pTipoFile not in ('02','04') then
       verrmsg := 'error Tipo de Archivo incorrecto [02=Detalle Liquidacion/04=Detalle Compensacion]';
       verrcod := '99';
       raise efinerror;
    end if;

    /* -- Verifica que exista informacion para la Fecha de Sesion y Moneda*/


    vPaso   := 'Paso 03';
    pqmonproc.inslog(vidproc, 'M', 'inicia generacion de reporte'||pTipoFile);




    --************************************* Inicia Generacion de Reporte *****************************************
    vPaso   := 'Paso 03a';
    vRegEncoDetalle    := lpad('0',10,'0');
    vImpNetFindDetalle := lpad('0',17,'0');
    verrorCab          := lpad('0',3,'0');
    vfillerCab         := lpad(' ',573,' ');
    vPaso   := 'Paso 03f';

    IF pTipoFile = '04' THEN
        vFile   := 'CLCO'||pTipoFile||vCsb||vsfecha||'.DAT';
        vHrCierre  := ' ';
    ELSE
       vFile   := 'CLCO'||pTipoFile||vCsb||vsfecha||pCodHrCierre||'.DAT';
       vHrCierre  := pCodHrCierre;
    END IF;
    vIDFile :=  utl_file.fopen(vdirout,vfile,'w',32767);

    if not utl_file.is_open(vIDFile) then
       verrmsg := 'error al abrir <'||vdirout||'/'||vfile||'>';
       verrcod := '99';
       raise efinerror;
    end if;

    vPaso   := 'Paso 04';

    --- Bucle para Cabecera
    IF pTipoFile = '04' THEN
       for d in cur_detalle_04 loop
           vNumRegEnviados := vNumRegEnviados +1;
           vSumImpEnviado  := vSumImpEnviado + d.S097_aux;
           -- Busca importe de registro N
           IF (d.cod_EntAdq = 'BM' and d.cod_entidad = '0108')OR
              (d.cod_EntAdq = 'BP' and d.cod_entidad = '0105') THEN
                 IF (d.tipmov = 'c' and vflg_tipmovc = '0') OR
                    (d.tipmov = 'd' and vflg_tipmovd = '0') then
                     vS097 :=  f_Getimpneto (d.cod_EntAdq , NULL, pFecSesion, d.tipmov, d.cod_moneda ) ;
                     vNumRegEnviados := vNumRegEnviados +1;
                     vSumImpEnviado  := vSumImpEnviado - vS097;
                     if d.tipmov = 'c' then vflg_tipmovc := '1'; end if;
                     if d.tipmov = 'd' then vflg_tipmovd := '1'; end if;
                 END IF;
           END IF;
       end loop;

    ELSIF pTipoFile = '02' THEN
       for d in cur_detalle_02 loop
           vNumRegEnviados := vNumRegEnviados +1;
           vSumImpEnviado  := vSumImpEnviado + d.S097_aux;
       end loop;
    END IF;

    vNumRegEnviados_aux := lpad(to_char(nvl(vNumRegEnviados,0)),10,0) ;

   /* if sign(vSumImpEnviado)= -1 then
       vSumImpEnviado_aux  := 'C'||lpad(abs(nvl(vSumImpEnviado,0)),16,0);
    else
       vSumImpEnviado_aux := 'D'||lpad(abs(nvl(vSumImpEnviado,0)),16,0);
    end if;*/
      
    if sign(vSumImpEnviado)= 1 then
       vSumImpEnviado_aux  := 'D'||lpad(abs(nvl(vSumImpEnviado,0)),16,0); /* Estaba en 16 -- 08/04/2018 TST*/
    else
       vSumImpEnviado_aux := 'C'||lpad(abs(nvl(vSumImpEnviado,0)),16,0); /* Estaba en 16 -- 08/04/2018 TST*/
    end if;

    vstr := 'A' ||vsfecha             ||vHrCierre         ||vNumRegEnviados_aux
                ||vSumImpEnviado_aux  ||vRegEncoDetalle   ||vImpNetFindDetalle
                ||verrorCab           ||vfillerCab;

    PutTXT(vstr);

    --- Bucle para Detalle
    vflg_tipmovc := '0';
    vflg_tipmovd := '0';
    IF pTipoFile = '04' THEN

       for d in cur_detalle_04 loop
          If d.cod_entadq = 'BM' then
             vCsb := '0105';
          else
             vCsb := '0108';
          end if;
           vstr := 'D'||vCsb||d.cod_entidad
                 ||UPPER(d.tipmov)||d.Num_lote
                 ||'E'||d.cod_moneda
                 ||d.S074||d.S075
                 ||d.S076||d.S077
                 ||d.S086||d.S087
                 ||d.S088||d.S089
                 ||d.S097||d.S105
                 ||d.S106||d.S107
                 ||d.S108||d.S110
                 ||d.S109||d.S111
                 ||d.S112||d.S113
                 ||d.S114||d.S115
                 ||d.S116||d.S117
                 ||d.S118||d.S119
                 ||d.S120||d.S121
                 ||d.S122||d.S123
                 ||d.S124||d.S125
                 ||d.S126||d.S127
                 ||d.S128||d.S129
                 ||d.S130||d.S131
                 ||d.S132||lpad(' ',114,' ');
           PutTXT(vstr);
           IF (d.cod_EntAdq = 'BM' and d.cod_entidad = '0108')OR
              (d.cod_EntAdq = 'BP' and d.cod_entidad = '0105') THEN
                 IF (d.tipmov = 'c' and vflg_tipmovc = '0') OR
                    (d.tipmov = 'd' and vflg_tipmovd = '0') THEN
                     vS097 :=  f_Getimpneto (d.cod_EntAdq , NULL, pFecSesion, d.tipmov, d.cod_moneda ) ;
                     vNumRegEnviados := vNumRegEnviados +1;
                     vSumImpEnviado  := vSumImpEnviado + vS097;
                     if d.tipmov = 'c' then vflg_tipmovc := '1'; end if;
                     if d.tipmov = 'd' then vflg_tipmovd := '1'; end if;
                     /*if sign(vS097)= -1 then
                        vC097 := 'D'||lpad(abs(vS097),16,'0');
                     else
                        vC097 := 'C'||lpad(abs(vS097),16,'0');
                     end if;*/
                     if sign(vS097)= 1 then
                        vC097 := 'C'||lpad(abs(vS097),16,'0');
                     else
                        vC097 := 'D'||lpad(abs(vS097),16,'0');
                     end if;
                     if d.cod_entadq = 'BM' then
                        vEntadq_aux := '0108';
                     else
                        vEntadq_aux := '0105' ;
                     end if;
                     vstr :=  'N' ||vCsb  ||vEntadq_aux|| UPPER(d.tipmov) ||d.Num_lote ||'E' ||d.cod_moneda
                     ||'0000000000'||'0000000000'||'0000000000'||'0000000000'
                     ||'0000000000000000'||'0000000000000000'||'0000000000000000'||'0000000000000000'
                     ||vC097
                     ||'0000000000000000'||'0000000000000000'
                     ||'0000000000'||'0000000000'
                     ||'0000000000000000'||'0000000000000000'
                     ||'0000000000'||'0000000000'
                     ||'0000000000000000'||'0000000000000000'
                     ||'0000000000'||'0000000000000000'
                     -- Adicionales (del S117 al S122)
                     ||'0000000000000000'||'0000000000000000'||'0000000000000000'||'0000000000000000'||'0000000000000000'
                     ||'0000000000'
                     -- Adicionales (del S123 al S132)
                     ||'0000000000000000'||'0000000000'||'0000000000000000'||'0000000000'
                     ||'0000000000000000'||'0000000000000000'||'0000000000'||'0000000000000000'||'0000000000'||'0000000000000000'
                     ||lpad(' ',114,' ');
                     PutTXT(vstr);
                 END IF;
           END IF;

       end loop;
    ELSIF pTipoFile = '02' THEN
       for d in cur_detalle_02 loop
           vstr :=  'D' ||vCsb  ||d.cod_entidad || UPPER(d.tipmov) ||d.Num_lote ||'E' ||d.cod_moneda
                 ||d.S074    ||d.S075
                 ||d.S076    ||d.S077
                 ||d.S086    ||d.S087
                 ||d.S088    ||d.S089
                 ||d.S097    ||d.S105
                 ||d.S106    ||d.S107
                 ------ crosadof -> Inc. 1105197:
                 --- Se esta mostrando de manera incorrecta en el archivo CLCO02 el orden de los campos S109 y S110,
                 --- Este error lo han notado en cuanto el Banco Provincial ha empezado a recoger este archivo desde el servidor XCOM
                 --- Se ha invertido el orden de los campos, dejando en observacion si este mismo problema se va a presentar
                 --- para el Banco Mercantil
                 --- Antes
                 --- ||d.S108    ||d.S110
                 --- ||d.S109    ||d.S111
                 --- Ahora
                 ||d.S108    ||d.S109
                 ||d.S110    ||d.S111
                 ------ fin crosadof
                 ||d.S112    ||d.S113
                 ||d.S114    ||d.S115
                 ||d.S116    ||lpad(' ',340,' '); -- antes era 84
           PutTXT(vstr);
       end loop;
    END IF;

    utl_file.fCLOSE(vIDFile);

  --Fin de Proceso
  pqmonproc.inslog(vidproc, 'M', 'Termino de Proceso');
  vretc :=pqmonproc.updmonproc (vidproc, 'F');
  RETURN '0~';

exception
        when efinerror then
             pqmonproc.inslog(vidproc, 'E', verrmsg);
             vretc :=pqmonproc.updmonproc (vidproc, 'E', verrcod);
             RAISE_APPLICATION_ERROR (-20000, verrmsg);
             RETURN 'E' || TRIM(RPAD(verrmsg,69)) || '~';
        when others then
             voracode:=sqlcode;
             voraerr:=substr(sqlerrm,1,200);
             pqmonproc.inslog(vidproc, 'E', voraerr);
             vretc :=pqmonproc.updmonproc (vidproc, 'E', '99');
             dbms_output.put_line('fin error vretc: '||vretc);
             dbms_output.put_line('fin error voraerr: '||voraerr);
             RAISE_APPLICATION_ERROR (-20001, vpaso||'-'||voraerr);
             RETURN 'Error de Base de Datos (' || voracode || '). Avisar a Soporte de Aplicaciones.~';

END;

FUNCTION f_Getimpneto (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE, ptipmov CHAR , pCodMoneda CHAR ) RETURN NUMBER
IS
   vImpNeto_emi number := 0;
   vImpNeto_adq number := 0;
   vImpFinal number    := 0;

BEGIN

IF (pCodHrCierre IS NOT NULL) THEN

   IF pEntAdq = 'BP' THEN

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_emi
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_hrcierre = pCodHrCierre
          AND cod_entidad = '0108'
          AND cod_entadq = 'BM'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_adq
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_hrcierre = pCodHrCierre
          AND cod_entidad = '0105'
          AND cod_entadq = 'BP'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

   END IF ;

   IF pEntAdq = 'BM' THEN

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_emi
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_hrcierre = pCodHrCierre
          AND cod_entidad = '0105'
          AND cod_entadq = 'BP'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov ;

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_adq
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_hrcierre = pCodHrCierre
          AND cod_entidad = '0108'
          AND cod_entadq = 'BM'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

   END IF;

ELSE

   IF pEntAdq = 'BP' THEN

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_emi
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_entidad = '0108'
          AND cod_entadq = 'BM'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_adq
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_entidad = '0105'
          AND cod_entadq = 'BP'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

   END IF ;

   IF pEntAdq = 'BM' THEN

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_emi
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_entidad = '0105'
          AND cod_entadq = 'BP'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov ;

      SELECT NVL(SUM(imp_neto_signo_comp),0)
         INTO vImpNeto_adq
         FROM clr_resumen_platco
         WHERE fec_sesion = pFecSesion
          AND cod_entidad = '0108'
          AND cod_entadq = 'BM'
          AND cod_moneda =  pCodMoneda
          AND tipmov = ptipmov;

   END IF;

END IF;

   vImpFinal := NVL(vImpNeto_emi,0) - NVL(vImpNeto_adq,0);

   RETURN vImpFinal;

EXCEPTION
   WHEN others  THEN
      RETURN  0;
END;


FUNCTION f_GetNumero( ptipo         VARCHAR2,
                      pidemen_p00    NUMBER,
                      pcodpro_p03    VARCHAR2,
                      pimptra_p04    NUMBER,
                      psigcu1_p46    VARCHAR2,
                      pimpcu1_p46    NUMBER) RETURN NUMBER IS

vNumero     NUMBER;

BEGIN
    vNumero := 0;

    IF NOT(pidemen_p00 IS NULL) THEN
        CASE ptipo
            WHEN 'S115' THEN
               IF   (pidemen_p00=1240 AND TO_NUMBER(pcodpro_p03) between '000000' and '190000')
                 OR (pidemen_p00 in (1440,1444) AND TO_NUMBER(pcodpro_p03) between '200000' and '290000')
                 OR (pidemen_p00=1744 AND psigcu1_p46= 'D')
               /*OR (pidemen_p00=1240 AND TO_NUMBER(pcodpro_p03) between '200000' and '290000')
                 OR (pidemen_p00=1744 AND psigcu1_p46= 'C') */ THEN
                 vNumero:=1;
               END IF;
            WHEN 'S122' THEN
               IF   (pidemen_p00=1240 AND TO_NUMBER(pcodpro_p03) between '200000' and '290000')
                 OR (pidemen_p00 in (1440,1444) AND TO_NUMBER(pcodpro_p03) between '000000' and '190000')
                 OR (pidemen_p00=1744 AND psigcu1_p46= 'C')
                /*OR (pidemen_p00=1744 AND psigcu1_p46= 'D')
                 OR (pidemen_p00=1240 AND TO_NUMBER(pcodpro_p03) between '000000' and '190000')*/ THEN
                 vNumero:=1;
               END IF;
            WHEN 'S120_1' THEN
               IF (pidemen_p00=1240) AND (TO_NUMBER(pcodpro_p03) between '200000' and '290000') THEN
                  vNumero := TO_NUMBER(pimptra_p04);
               END IF;
            WHEN 'S120_2' THEN
                IF (pidemen_p00 in (1440,1444))  AND (TO_NUMBER(pcodpro_p03) between '000000' and '190000') THEN
                  vNumero := TO_NUMBER(pimptra_p04);
                END IF;
             WHEN 'S120_3' THEN
                IF (pidemen_p00=1744) AND (psigcu1_p46= 'C') THEN
                   vNumero := TO_NUMBER(pimpcu1_p46);
                END IF;
            WHEN 'S116_1' THEN
               IF (pidemen_p00=1240) AND (TO_NUMBER(pcodpro_p03) between '000000' and '190000') THEN
                  vNumero := TO_NUMBER(pimptra_p04);
               END IF;
            WHEN 'S116_2' THEN
               IF (pidemen_p00 in (1440,1444)) AND (TO_NUMBER(pcodpro_p03) between '200000' and '290000') THEN
                  vNumero := TO_NUMBER(pimptra_p04);
               END IF;
            WHEN 'S116_3' THEN
                IF (pidemen_p00=1744) AND (psigcu1_p46= 'D') THEN
                   vNumero := TO_NUMBER(pimpcu1_p46);
                END IF;
            WHEN 'S121_1' THEN
               IF (pidemen_p00=1240) AND (TO_NUMBER(pcodpro_p03) between '200000' and '290000') THEN
                  vNumero := TO_NUMBER(pimpcu1_p46);
               END IF;
            WHEN 'S121_2' THEN
               IF (pidemen_p00 in (1440,1444)) AND (TO_NUMBER(pcodpro_p03) between '000000' and '190000') THEN
                  vNumero := TO_NUMBER(pimpcu1_p46);
               END IF;
            WHEN 'S119_1' THEN
               IF (pidemen_p00=1240) AND (TO_NUMBER(pcodpro_p03) between '000000' and '190000') THEN
                  vNumero := TO_NUMBER(pimpcu1_p46);
               END IF;
            WHEN 'S119_2' THEN
               IF (pidemen_p00 in (1440,1444)) AND (TO_NUMBER(pcodpro_p03) between '200000' and '290000') THEN
                  vNumero := TO_NUMBER(pimpcu1_p46);
               END IF;
        END CASE;
    END IF;

    RETURN vNumero;
END;


FUNCTION f_GetComisionMC  (pPDS0158_P48 CHAR, pP28SESION CHAR, pP04IMPTRA CHAR,
               pP48TIPTRA CHAR, pP18CODACT CHAR, pP48TIPMOV CHAR) RETURN NUMBER
is
vComPCTL  NUMBER:=0;
vComFIJA  NUMBER:=0;
vTC       NUMBER:=0;
vComImpTotal NUMBER:=0;
vComImpFija NUMBER:=0;
begin
  -- Obtiene las Comisiones Porcentual y Fija
  BEGIN
    SELECT NVL(COM_PCTL,0)/100 COM_PCTL,
           NVL(COM_FIJA,0)
      INTO vComPCTL,
           vComFIJA
      FROM CFG_MCIRD
     WHERE COD_TIPTRA = SUBSTR(pP48TIPTRA,1,2)
       AND COD_IRD = pPDS0158_P48
       AND FEC_INIVIG <= pP28SESION
       AND FEC_FINVIG >= pP28SESION;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
         SELECT (DECODE(pP48TIPMOV,'c',TASA_CRE,'d',TASA_DEB)*0.8)/100 COM_PCTL
           INTO vComPCTL
           FROM MCC_PMP
          WHERE COD_MCC = pP18CODACT;
         vComFIJA:=0;
  END;
  -- Obtiene Tasa de Cambio
  -- vTC:=GetTC();
  -- vTC:=2.15;
  vTC := PQCOMERCIOS.GCW_F_GETMONEDATIPOCAMBIO('BM','MC', '840', PQCOMERCIOS.GCW_F_GETMONEDAVIG(pP28SESION),pP28SESION);
  -- Calcula Comisiones
  vComImpTotal :=ROUND(vComPCTL*pP04IMPTRA);

  vComImpFija:=(vComFIJA*vTC)*100;
  IF pP04IMPTRA <=0  THEN
     vComImpFija := -1* vComImpFija;
  END IF;
  vComImpTotal := vComImpTotal + vComImpFija;

  Return vComImpTotal;
end;


PROCEDURE P_COMP_GENDATRPT05   (pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1), pCodMoneda CHAR)
IS

vS097_BM_c  NUMBER := 0;
vS097_BM_d  NUMBER := 0;
vS097_BM_a  NUMBER := 0;
vS097_BM  NUMBER := 0;

vS097_BP_c  NUMBER := 0;
vS097_BP_d  NUMBER := 0;
vS097_BP_a  NUMBER := 0;
vS097_BP  NUMBER := 0;


BEGIN

DELETE CLR_RESUMEN_CLCO05 WHERE fec_sesion = pFecSesion AND cod_moneda = pCodMoneda;

vS097_BM_c :=  f_Getimpneto ('BM' , pCodHrCierre, pFecSesion, 'c' , pcodmoneda) ;
vS097_BM_d :=  f_Getimpneto ('BM' , pCodHrCierre, pFecSesion, 'd' , pcodmoneda) ;
vS097_BM_a :=  f_Getimpneto ('BM' , pCodHrCierre, pFecSesion, 'a' , pcodmoneda) ;

vS097_BM := vS097_BM_c + vS097_BM_d + vS097_BM_a ;

vS097_BP_c :=  f_Getimpneto ('BP' , pCodHrCierre, pFecSesion, 'c' , pcodmoneda) ;
vS097_BP_d :=  f_Getimpneto ('BP' , pCodHrCierre, pFecSesion, 'd' , pcodmoneda) ;
vS097_BP_a :=  f_Getimpneto ('BP' , pCodHrCierre, pFecSesion, 'a' , pcodmoneda) ;

vS097_BP := vS097_BP_c + vS097_BP_d + vS097_BP_a ;



INSERT INTO CLR_RESUMEN_CLCO05 (cod_entidad , fec_sesion, cod_moneda , imp_neto, imp_neto_signo)
  VALUES (105,pFecSesion, pCodMoneda, vS097_BM , vS097_BM);

INSERT INTO CLR_RESUMEN_CLCO05 (cod_entidad , fec_sesion, cod_moneda , imp_neto, imp_neto_signo)
  VALUES (108,pFecSesion, pCodMoneda, vS097_BP , vS097_BP);

COMMIT;

END;


PROCEDURE P_CLR_GENDATRPT_PLATCO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1))
IS
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure    : P_CLR_GENDATRPT_PLATCO
-- Proposito    : Generar los datos en las tablas CLR_RESUMEN y CLR_RPTOUT102
--                luego de haberse efectuado el Proceso de Clearing
--
-- Persona     Fecha     Comentarios
-- ---------   --------  -------------------------------------------
-- SSM         20031114  Codigo Inicial
-- JPC         20070415  Adapatacion a PMPV
-- JMG         20080703  Actualizacion y Errores controlados
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

vCodMoneda      CHAR(3) := '937' ;
vDescMoneda     VARCHAR2(20) := 'Bolivares';
vIdProc         NUMBER;
vRetC           VARCHAR2(100);
vOraErr         VARCHAR2(200);

PROCEDURE InsDatRptRes IS
BEGIN
    -- Elimina informacion anterior
    pqmonproc.inslog(vIdProc, 'M', 'Eliminando informacion anterior en clr_resumen_platco.');
    DELETE CLR_RESUMEN_PLATCO
     WHERE fec_sesion   = pFecSesion
       AND cod_hrcierre = pCodHrCierre
       AND cod_entadq   = pEntAdq;

    -- Emisor
    pqmonproc.inslog(vIdProc, 'M', 'Generando datos de Emisor.');
    FOR r IN ( SELECT SUBSTR(idedes_s93_accc,3,4) EntDest,
                      signox_s97_accc,
                      inlote_p29_accc,
                      NVL((numabo_s74_accc),0)+NVL((numana_s75_accc),0)+
                      NVL((numcar_s76_accc),0)+NVL((numanc_s77_accc),0)+
                      NVL((nchaab_s107_accc),0)+NVL((nchaca_s108_accc),0)+
                      NVL((nabomis_s111_accc),0)+NVL((ncarmis_s112_accc),0) NumOpe,
                      NVL((netoxx_s97_accc),0) ImpNeto,
                      NVL((DECODE(signox_s97_accc,'D',(-1)*netoxx_s97_accc,'C',netoxx_s97_accc)),0) ImpNetoSigno,
                      NVL((numabo_s74_accc),0) numabo_s74_accc,
                      NVL((numana_s75_accc),0) numana_s75_accc,
                      NVL((numcar_s76_accc),0) numcar_s76_accc,
                      NVL((numanc_s77_accc),0) numanc_s77_accc,
                      NVL((impabo_s86_accc),0) impabo_s86_accc,
                      NVL((impana_s87_accc),0) impana_s87_accc,
                      NVL((impcar_s88_accc),0) impcar_s88_accc,
                      NVL((impanc_s89_accc),0) impanc_s89_accc,
                      NVL((ichaab_s105_accc),0) ichaab_s105_accc,
                      NVL((ichaca_s106_accc),0) ichaca_s106_accc,
                      NVL((nchaab_s107_accc),0) nchaab_s107_accc,
                      NVL((nchaca_s108_accc),0) nchaca_s108_accc,
                      NVL((cuoabo_s109_accc),0) cuoabo_s109_accc,
                      NVL((cuocar_s110_accc),0) cuocar_s110_accc,
                      NVL((nabomis_s111_accc),0) nabomis_s111_accc,
                      NVL((ncarmis_s112_accc),0) ncarmis_s112_accc,
                      NVL((abomis_s113_accc),0) abomis_s113_accc,
                      NVL((carmis_s114_accc),0) carmis_s114_accc,
                      NVL((nrepabo_s129_accc),0) nrepabo_s129_accc,
                      NVL((repabo_s130_accc),0) repabo_s130_accc,
                      NVL((nrepcar_s131_accc),0) nrepcar_s131_accc,
                      NVL((repcar_s132_accc),0) repcar_s132_accc,
                      tipmov, moneda_pxxx_accc
                 FROM CLR_LETOTA_PLATCO
                WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                  AND cod_hrcierre = pCodHrCierre
                  AND cod_entadq = pEntAdq
             ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
    LOOP

       DBMS_OUTPUT.PUT_LINE('r.entdest: '||r.entdest);
       DBMS_OUTPUT.PUT_LINE('r.cuoabo_s109_accc: '||r.cuoabo_s109_accc);

        INSERT INTO clr_resumen_platco
           (fec_sesion,         cod_hrcierre,       cod_moneda,             tipo_lote,
            cod_entadq,         cod_entidad,        num_lote,               tipo_saldo,
            num_oper,           imp_neto,           imp_neto_signo,         tipo_saldo_comp,
            num_oper_comp,      imp_neto_comp,      imp_neto_signo_comp,    numabo_s74_accc,
            numana_s75_accc,    numcar_s76_accc,    numanc_s77_accc,        impabo_s86_accc,
            impana_s87_accc,    impcar_s88_accc,    impanc_s89_accc,        ichaab_s105_accc,
            ichaca_s106_accc,   nchaab_s107_accc,   nchaca_s108_accc,       cuoabo_s109_accc,
            cuocar_s110_accc,   nabomis_s111_accc,  ncarmis_s112_accc,      abomis_s113_accc,
            carmis_s114_accc,   nrepabo_s129_accc,  repabo_s130_accc,
            nrepcar_s131_accc,  repcar_s132_accc,   tipmov )
        VALUES
           (pfecsesion,         pcodhrcierre,       r.moneda_pxxx_accc,     'E',
            pentadq,            r.entdest,          r.inlote_p29_accc,      r.signox_s97_accc,
            r.numope,           r.impneto,          r.impnetosigno,         r.signox_s97_accc,
            r.numope,           r.impneto,          r.impnetosigno,         r.numabo_s74_accc,
            r.numana_s75_accc,  r.numcar_s76_accc,  r.numanc_s77_accc,      r.impabo_s86_accc,
            r.impana_s87_accc,  r.impcar_s88_accc,  r.impanc_s89_accc,      r.ichaab_s105_accc,
            r.ichaca_s106_accc, r.nchaab_s107_accc, r.nchaca_s108_accc,     r.cuoabo_s109_accc,
            r.cuocar_s110_accc, r.nabomis_s111_accc,r.ncarmis_s112_accc,    r.abomis_s113_accc,
            r.carmis_s114_accc, r.nrepabo_s129_accc,r.repabo_s130_accc,
            r.nrepcar_s131_accc,r.repcar_s132_accc, r.tipmov );
    END LOOP;

    -- Merchant
    pqmonproc.inslog(vIdProc, 'M', 'Generando datos de Merchant.');
    FOR r IN ( SELECT SUBSTR(idedes_s93_accc,3,4) EntDest,
                      signox_s97_accc,
                      inlote_p29_accc,
                      NVL((numabo_s74_accc),0)+NVL((numana_s75_accc),0)+
                      NVL((numcar_s76_accc),0)+NVL((numanc_s77_accc),0)+
                      NVL((nchaab_s107_accc),0)+NVL((nchaca_s108_accc),0)+
                      NVL((nabomis_s111_accc),0)+NVL((ncarmis_s112_accc),0) NumOpe,
                      NVL((netoxx_s97_accc),0) ImpNeto,
                      NVL((DECODE(signox_s97_accc,'D',(-1)*netoxx_s97_accc,'C',netoxx_s97_accc)),0) ImpNetoSigno,
                      NVL((numabo_s74_accc),0) numabo_s74_accc,
                      NVL((numana_s75_accc),0) numana_s75_accc,
                      NVL((numcar_s76_accc),0) numcar_s76_accc,
                      NVL((numanc_s77_accc),0) numanc_s77_accc,
                      NVL((impabo_s86_accc),0) impabo_s86_accc,
                      NVL((impana_s87_accc),0) impana_s87_accc,
                      NVL((impcar_s88_accc),0) impcar_s88_accc,
                      NVL((impanc_s89_accc),0) impanc_s89_accc,
                      NVL((ichaab_s105_accc),0) ichaab_s105_accc,
                      NVL((ichaca_s106_accc),0) ichaca_s106_accc,
                      NVL((nchaab_s107_accc),0) nchaab_s107_accc,
                      NVL((nchaca_s108_accc),0) nchaca_s108_accc,
                      NVL((cuoabo_s109_accc),0) cuoabo_s109_accc,
                      NVL((cuocar_s110_accc),0) cuocar_s110_accc,
                      NVL((nabomis_s111_accc),0) nabomis_s111_accc,
                      NVL((ncarmis_s112_accc),0) ncarmis_s112_accc,
                      NVL((abomis_s113_accc),0) abomis_s113_accc,
                      NVL((carmis_s114_accc),0) carmis_s114_accc,
                      NVL((nrepabo_s129_accc),0) nrepabo_s129_accc,
                      NVL((repabo_s130_accc),0) repabo_s130_accc,
                      NVL((nrepcar_s131_accc),0) nrepcar_s131_accc,
                      NVL((repcar_s132_accc),0) repcar_s132_accc,
                      tipmov, moneda_pxxx_accc
                 FROM CLR_LMTOTA_PLATCO
                WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                  AND cod_hrcierre = pCodHrCierre
                  AND cod_entadq = pEntAdq
             ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
    LOOP
        INSERT INTO clr_resumen_platco
           (fec_sesion,         cod_hrcierre,       cod_moneda,             tipo_lote,
            cod_entadq,         cod_entidad,        num_lote,               tipo_saldo,
            num_oper,           imp_neto,           imp_neto_signo,         tipo_saldo_comp,
            num_oper_comp,      imp_neto_comp,      imp_neto_signo_comp,    numabo_s74_accc,
            numana_s75_accc,    numcar_s76_accc,    numanc_s77_accc,        impabo_s86_accc,
            impana_s87_accc,    impcar_s88_accc,    impanc_s89_accc,        ichaab_s105_accc,
            ichaca_s106_accc,   nchaab_s107_accc,   nchaca_s108_accc,       cuoabo_s109_accc,
            cuocar_s110_accc,   nabomis_s111_accc,  ncarmis_s112_accc,      abomis_s113_accc,
            carmis_s114_accc,   nrepabo_s129_accc,  repabo_s130_accc,
            nrepcar_s131_accc,  repcar_s132_accc,   tipmov )
        VALUES
           (pfecsesion,         pcodhrcierre,       r.moneda_pxxx_accc,     'M',
            pentadq,            r.entdest,          r.inlote_p29_accc,      r.signox_s97_accc,
            r.numope,           r.impneto,          r.impnetosigno,         r.signox_s97_accc,
            r.numope,           r.impneto,          r.impnetosigno,         r.numabo_s74_accc,
            r.numana_s75_accc,  r.numcar_s76_accc,  r.numanc_s77_accc,      r.impabo_s86_accc,
            r.impana_s87_accc,  r.impcar_s88_accc,  r.impanc_s89_accc,      r.ichaab_s105_accc,
            r.ichaca_s106_accc, r.nchaab_s107_accc, r.nchaca_s108_accc,     r.cuoabo_s109_accc,
            r.cuocar_s110_accc, r.nabomis_s111_accc,r.ncarmis_s112_accc,    r.abomis_s113_accc,
            r.carmis_s114_accc, r.nrepabo_s129_accc,r.repabo_s130_accc,
            r.nrepcar_s131_accc,r.repcar_s132_accc, r.tipmov );

    END LOOP;
END;

BEGIN
    /* ID DE PROCESO
    ****************/
    vIdProc:= pqmonproc.InsMonProc ('PCLRPLATCO');
    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIdProc, 'M', 'Inicia insercion de datos en clr_resumen_platco');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre );
    InsDatRptRes;
    pqmonproc.inslog(vIdProc, 'M', 'Termino insercion de datos en clr_resumen_platco');
    /* FIN DE PROCESO
    *****************/
    vRetC  := pqmonproc.updmonproc (vIdProc, 'F');
EXCEPTION
    WHEN OTHERS THEN
         ROLLBACK;
         vOraErr:=SUBSTR(SQLERRM,1,200);
         pqmonproc.inslog(vIdProc, 'E', vOraErr);
         vRetC  :=pqmonproc.updmonproc (vIDProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);
END; -- P_CLR_GENDATRPT_PLATCO


PROCEDURE P_COMP_GENDATRPT_PLATCO (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1))
IS
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure    : P_COMP_GENDATRPT_PLATCO
-- Proposito    : Generar Compensacion Platco
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    vS105   NUMBER(16) := 0;
    vS106   NUMBER(16) := 0;
    vS107   NUMBER := 0;
    vS108   NUMBER := 0;

    vimp_neto_comp          NUMBER(16) := 0;
    vimp_neto_signo_comp    NUMBER(16) := 0;

    vS86    NUMBER(16) := 0;
    vS87    NUMBER(16) := 0;
    vS88    NUMBER(16) := 0;
    vS89    NUMBER(16) := 0;
    vS109   NUMBER(16) := 0;
    vS110   NUMBER(16) := 0;
    vS113   NUMBER(16) := 0;
    vS114   NUMBER(16) := 0;
    vS115   NUMBER(16) := 0;
    vS116   NUMBER(16) := 0;    
    vS117   NUMBER(16) := 0;
    vS118   NUMBER(16) := 0;
    vS119   NUMBER(16) := 0;
    vS120   NUMBER(16) := 0;
    vS121   NUMBER(16) := 0;
    vS122   NUMBER(16) := 0;
    vS123   NUMBER(16) := 0;
    vS124   NUMBER(16) := 0;
    vS125   NUMBER(16) := 0;    
    vS126   NUMBER(16) := 0;
    vS127   NUMBER(16) := 0;
    vS128   NUMBER(16) := 0;
    vS129   NUMBER(10) := 0;
    vS130   NUMBER(16) := 0;
    vS131   NUMBER(10) := 0;
    vS132   NUMBER(16) := 0;
    vS133   NUMBER(10) := 0;
    vS134   NUMBER(10) := 0;
    vS135   NUMBER(16) := 0;
    vS136   NUMBER(16) := 0;
    vS137   NUMBER(10) := 0;
    vS138   NUMBER(10) := 0;
    vS139   NUMBER(16) := 0;
    vS140   NUMBER(16) := 0;
            
    vAux            VARCHAR2(4);
    vCod_entidadM   NUMBER;
    vTipoSaldo      CHAR(1);

    vIdProc       NUMBER;
    vRetC         VARCHAR2(100);
    vOraCode      NUMBER;
    vOraErr       VARCHAR2(200);
    vErrMsg       VARCHAR2(200);
    vErrCod       VARCHAR2(2);

BEGIN

    /* ID DE PROCESO
    ****************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc('**',pFecSesion,'SES','PCOMPPLATCO');

    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIdProc, 'M', 'Inicia Compensacion platco.');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre );

    -- Procesa cobros Pagos Recibidos
    pqmonproc.inslog(vIdProc, 'M', 'Procesando cobros pagos recibidos.');
    FOR rCont IN ( SELECT SUM (DECODE (rr.tipo_operacion, 'C', 1, 0)) s111,
                          SUM (DECODE (rr.tipo_operacion, 'P', 1, 0)) s112, 
                          SUM (DECODE (rr.tipo_operacion, 'C', rr.monto_work, 0)) s113,
                          SUM (DECODE (rr.tipo_operacion, 'P', rr.monto_work, 0)) s114,                       
                          MONEDA_WORK cod_moneda, 
                          '511' num_lote, 
                          'c' tipmov, 
                          DECODE(COD_ENTADQ,'BM',105,'BP',108) cod_entidad
                     FROM cobros_pagos_recibidos rr
                    WHERE origen            = 'IL'
                      AND moneda_work       = '937'
                      AND rr.cod_entadq     = pEntAdq
                      AND rr.fecha_registro = pFecSesion
                 GROUP BY MONEDA_WORK,COD_ENTADQ ) 
    LOOP              
    BEGIN
        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(ichaca_s106_accc, 0),
               NVL(cuoabo_s109_accc, 0),        NVL(cuocar_s110_accc, 0),
               NVL(rechazos_s116_accc, 0),      NVL(cuochaab_s117_accc, 0),
               NVL(cuochaca_s118_accc, 0),      NVL(cuorechazos_s119_accc, 0),
               NVL(rechcar_s120_accc, 0),       NVL(cuorechabo_s121_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),       NVL(rcarmis_s136_accc, 0),
               NVL(rabomis_s_s139_accc, 0),     NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs106,  vs109,  vs110,
               vs116,   vs117,  vs118,  vs119,  vs120,  vs121,  vs123,  vs125,
               vs127,   vs128,  vs130,  vs132,  vs135,  vs136,  vs139,  vs140
           FROM clr_resumen_platco
         WHERE cod_hrcierre     = pcodhrcierre
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = rcont.tipmov
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = rcont.cod_moneda
           AND tipo_lote        = 'E';

        vimp_neto_signo_comp :=  vS86  + vS87 - vS88 - vS89 + vS105 - vS106 + vS109 - vS110 + abs(rCont.S113) - abs(rCont.S114) + vS116 - vS117 + vS118 - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS129 - vS130 + vS135 - vS136 + vS139 - vS140;
           
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;

        UPDATE clr_resumen_platco
           SET nabomis_s111_accc    = rcont.s111,
               ncarmis_s112_accc    = rcont.s112,
               abomis_s113_accc     = abs(rcont.s113),
               carmis_s114_accc     = abs(rcont.s114),
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_hrcierre = pcodhrcierre
           AND cod_entadq   = pentadq
           AND fec_sesion   = pFecSesion
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E';
           
    EXCEPTION
        WHEN no_data_found THEN 
             vimp_neto_signo_comp :=  rCont.S113 - rCont.S114;           
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;
           
             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entidad,
                num_lote, tipo_saldo, num_oper, imp_neto, imp_neto_signo,
                tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES (pFecSesion, pcodhrcierre, rcont.cod_moneda, 'E', rcont.cod_entidad,
                rcont.num_lote, 'C', 0, 0, 0,
                vtiposaldo, rcont.s111 + rcont.s112, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                rcont.s111, rcont.s112, abs(rcont.s113),
                abs(rcont.s114), pentadq, rcont.tipmov, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0 );
    END;
    END LOOP;
     
    -- Procesa reversos de contracargos
    -- genera registro de Emisor  para reversiones de contracargos
    pqmonproc.inslog(vIdProc, 'M', 'Procesando registro de Emisor para reversiones de contracargos.');
    FOR rCont IN ( SELECT COUNT(*) s108,--s107,
                          SUM(dm.incoming_dest_amount) s106,--s105,
                          SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)), -1*TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0))) +
                          DECODE(SUBSTR(dm.INCOMING_P46TCUOT02,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT02,4),0)), -1*TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT02,4),0)))) s118,
                          doo.p29inlote num_lote,
                          doo.p48tipmov tipmov,
                          doo.cod_bcoemi cod_entidad ,
                          cp.cod_horacierre cod_hrcierre,
                          dm.incoming_dest_curren_code cod_moneda
                     FROM DISPUTAS_MASTER dm, 
                          DISPUTAS_OPERACION_ORIGINAL doo,
                          comercios_pmp cp
                    WHERE dm.Disputa_type   IN (35,36)
                      AND dm.cod_entadq     = pEntAdq
                      AND dm.incoming_date  = pFecSesion
                      AND dm.movdis_id      = doo.movdis_id
                      AND doo.cod_comercio  = cp.cod_comercio
                      AND doo.cod_bcoemi    IN (105,108)
                 GROUP BY doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code ) 
    LOOP     
    BEGIN     
        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(cuoabo_s109_accc, 0),
               NVL(cuocar_s110_accc, 0),        NVL(abomis_s113_accc, 0),
               NVL(carmis_s114_accc, 0),        NVL(rechazos_s116_accc, 0),
               NVL(cuochaab_s117_accc, 0),      NVL(cuorechazos_s119_accc, 0),
               NVL(rechcar_s120_accc, 0),       NVL(cuorechabo_s121_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),       NVL(rcarmis_s136_accc, 0),
               NVL(rabomis_s_s139_accc, 0),     NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs109,  vs110,  vs113,
               vs114,   vs116,  vs117,  vs119,  vs120,  vs121,  vs123,  vs125,
               vs127,   vs128,  vs130,  vs132,  vs135,  vS136,  vS139,  vs140
          FROM clr_resumen_platco
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E'
           AND num_lote     =  DECODE(rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516)
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND fec_sesion   = pfecsesion;

        vS106 := abs(rCont.S106);
        vS118 := abs(rCont.S118);
        
        vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 + vs105 - vS106 + vS109 - vS110 + vS113 - vS114 + vS116 - vS117 +  vS118 - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vs135 - vs136 + vS139 - vS140;

        DBMS_OUTPUT.PUT_LINE('vimp_neto_signo_comp:'||vimp_neto_signo_comp);
        
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;
     
        UPDATE clr_resumen_platco
           SET ichaca_s106_accc     = vS106,
               nchaca_s108_accc     = rcont.s108,
               cuochaca_s118_accc   = vS118,
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E'
           AND num_lote     = DECODE(rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516)
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND fec_sesion   = pfecsesion;
           
    EXCEPTION
        WHEN no_data_found THEN
             vimp_neto_signo_comp :=  - vS106 + vS118;

             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;
          
             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote,
                cod_entidad,
                num_lote,
                tipo_saldo, num_oper, imp_neto, imp_neto_signo, tipo_saldo_comp,
                num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc )
             VALUES 
                (pfecsesion, rcont.cod_hrcierre, rcont.cod_moneda, 'E',
                rcont.cod_entidad,
                DECODE(rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516),
                'C', 0, 0, 0, vtiposaldo,
                rcont.s108, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, vS106, 0,
                rcont.s108, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, 0,
                0, 0, vs118,
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0 );
    END;
    END LOOP;

    -- genera registro de merchant para reversiones de contracargos
    pqmonproc.inslog(vIdProc, 'M', 'Procesando registro de Merchant para reversiones de contracargos.');
    FOR rCont IN (SELECT COUNT(*) s108,--s107,
                     SUM(dm.incoming_dest_amount) s106,--s105,
                     SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)), -1*TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0))) +
                         DECODE(SUBSTR(dm.INCOMING_P46TCUOT02,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT02,4),0)), -1*TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT02,4),0)))) s118,
                     doo.p29inlote num_lote,
                     doo.p48tipmov tipmov,
                     doo.cod_bcoemi cod_entidad ,
                     cp.cod_horacierre cod_hrcierre,
                     dm.incoming_dest_curren_code cod_moneda,
                     substr(doo.p48tiptra,1,2) p48tiptra
                FROM DISPUTAS_MASTER dm, DISPUTAS_OPERACION_ORIGINAL doo,
                     comercios_pmp cp
               WHERE dm.Disputa_type    IN (35,36)
                 AND dm.cod_entadq      = pEntAdq
                 AND dm.incoming_date   = pFecSesion
                 AND dm.movdis_id       = doo.movdis_id
                 AND doo.cod_comercio   = cp.cod_comercio
                 AND doo.cod_bcoemi     IN (105,108)
            GROUP BY doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code , substr(doo.p48tiptra,1,2) )
    LOOP
    BEGIN
        vaux := '';
        IF pEntAdq = 'BM' THEN
            vaux := '98';
        ELSIF pEntAdq = 'BP' THEN
            vaux := '99';
        END IF;

        IF SUBSTR(rCont.p48tiptra,1,2) = '01' THEN
            vaux := '99'||vaux;
        ELSIF (SUBSTR(rCont.p48tiptra,1,2) = '06' OR SUBSTR(rCont.p48tiptra,1,2) = '10' )THEN
            vaux := '98'||vaux;
        ELSIF SUBSTR(rCont.p48tiptra,1,2) = '14' THEN
            vaux := '97'||vaux;
        ELSIF SUBSTR(rCont.p48tiptra,1,2) = '20' THEN
            vaux := '89'||vaux;
        END IF;

        vCod_entidadM := to_number(vaux) ;

        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(cuoabo_s109_accc, 0),
               NVL(cuocar_s110_accc, 0),        NVL(abomis_s113_accc, 0),
               NVL(carmis_s114_accc, 0),        NVL(rechazos_s116_accc, 0),
               NVL(cuochaab_s117_accc, 0),      NVL(cuochaca_s118_accc, 0),
               NVL(cuorechazos_s119_accc, 0),   NVL(rechcar_s120_accc, 0),
               NVL(cuorechabo_s121_accc, 0),    NVL(rechcar_s_s123_accc, 0),
               NVL(rechazos_s_s125_accc, 0),    NVL(cuorechazos_s_s127_accc, 0),
               NVL(cuorechabo_s_s128_accc, 0),  NVL(repabo_s130_accc, 0),        
               NVL(repcar_s132_accc, 0),        NVL(rabomis_s135_accc, 0),
               NVL(rcarmis_s136_accc, 0),       NVL(rabomis_s_s139_accc, 0),
               NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs109,  vs110,  vs113,
               vs114,   vs116,  vs117,  vs118,  vs119,  vs120,  vs121,  vs123,
               vs125,   vs127,  vs128,  vs130,  vs132,  vS135,  vS136,   vS139,  vS140          
          FROM clr_resumen_platco
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'M'
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = vcod_entidadm
           AND fec_sesion   = pfecsesion;

        vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - abs(rCont.S106) + vs105 + vS109 - vS110 + vS113 - vS114 + vS116 - vS117 + abs(rCont.S118) - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vS135 - vs136 + vS139 - vS140;
     
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'D';
        ELSE
            vTipoSaldo := 'C';
        END IF;
     
        vimp_neto_signo_comp := vimp_neto_signo_comp * -1 ;

        UPDATE clr_resumen_platco
           SET ichaca_s106_accc     = abs(rcont.s106),
               nchaca_s108_accc     = rcont.s108,
               cuochaca_s118_accc   = abs(rcont.s118),
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'M'
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = vcod_entidadm
           AND fec_sesion   = pfecsesion;

    EXCEPTION
        WHEN no_data_found THEN
             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote,
                cod_entidad, num_lote, tipo_saldo, num_oper, imp_neto,
                imp_neto_signo, tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES 
                (pfecsesion - 1, rcont.cod_hrcierre, rcont.cod_moneda, 'M',
                vcod_entidadm, rcont.num_lote, 'C', 0, 0,
                0, 'C', rcont.s108, abs(rcont.s106),
                -1 * abs(rcont.s106), 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, abs(rcont.s106), 0,
                rcont.s108, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, 0,
                0, 0, abs(rCont.s118),
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0 );
    END;
    END LOOP;

    -- Procesa Contracargos PRICE
    -- genera registro de Emisor para contracargos
    pqmonproc.inslog(vIdProc, 'M', 'Procesando registro de Emisor para contracargos.');
    FOR rCont IN ( SELECT COUNT(*) s107,--s108,
                          SUM(dm.incoming_dest_amount) s105,--s106 ,
                          SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'D', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)),0)) s117,--antes estaba en C
                          SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)),0)) s118,--antes estaba en D
                          doo.p29inlote num_lote,
                          doo.p48tipmov tipmov,
                          doo.cod_bcoemi cod_entidad ,
                          cp.cod_horacierre cod_hrcierre,
                          dm.incoming_dest_curren_code cod_moneda
                     FROM DISPUTAS_MASTER dm, DISPUTAS_OPERACION_ORIGINAL doo,
                          comercios_pmp cp
                    WHERE dm.Disputa_type   IN (15,16,17)
                      AND dm.cod_entadq     = pEntAdq
                      AND dm.incoming_date  = pFecSesion
                      AND dm.movdis_id      = doo.movdis_id
                      AND doo.cod_comercio  = cp.cod_comercio
                      AND doo.cod_bcoemi    IN (105,108)
                 GROUP BY doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code ) 
    LOOP
    BEGIN
        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaca_s106_accc, 0),        NVL(cuoabo_s109_accc, 0),
               NVL(cuocar_s110_accc, 0),        NVL(abomis_s113_accc, 0),
               NVL(carmis_s114_accc, 0),        NVL(rechazos_s116_accc, 0),
               NVL(cuorechazos_s119_accc, 0),   NVL(rechcar_s120_accc, 0),
               NVL(cuorechabo_s121_accc, 0),    NVL(rechcar_s_s123_accc, 0),
               NVL(rechazos_s_s125_accc, 0),    NVL(cuorechazos_s_s127_accc, 0),
               NVL(cuorechabo_s_s128_accc, 0),  NVL(repabo_s130_accc, 0),        
               NVL(repcar_s132_accc, 0),        NVL(rabomis_s135_accc, 0),
               NVL(rcarmis_s136_accc, 0),       NVL(rabomis_s_s139_accc, 0),
               NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs106,  vs109,  vs110,  vs113,
               vs114,   vs116,  vs119,  vs120,  vs121,  vs123,  vs125,  vs127,
               vs128,   vs130,  vs132,  vS135,  vS136,  vS139,  vS140          
          FROM clr_resumen_platco
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E'
           AND num_lote     = DECODE (rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516)
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND fec_sesion   = pfecsesion;

        vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - vS106 + abs(rCont.S105)  + vS109 - vS110 + vS113 - vS114 + vS116 - abs(rCont.S117) + abs(rCont.S118) - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vS135 - vs136 + vS139 - vS140;  
     
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;
     
        DBMS_OUTPUT.PUT_LINE('vimp_neto_signo_comp_contr: '||vimp_neto_signo_comp);
          
        UPDATE clr_resumen_platco
           SET ichaab_s105_accc     = abs(rcont.s105),
               nchaab_s107_accc     = rcont.s107,
               cuochaab_s117_accc   = abs(rcont.s117),
               cuochaca_s118_accc   = abs(rcont.s118),
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E'
           AND num_lote     = DECODE (rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516)
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND fec_sesion   = pfecsesion;

    EXCEPTION
        WHEN no_data_found THEN   
             vimp_neto_signo_comp :=  abs(rCont.S105) - abs(rCont.S117) + abs(rCont.S118);
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;
          
             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote,
                cod_entidad,
                num_lote,
                tipo_saldo, num_oper, imp_neto, imp_neto_signo, tipo_saldo_comp,
                num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc )
             VALUES 
                (pfecsesion, rcont.cod_hrcierre, rcont.cod_moneda, 'E',
                rcont.cod_entidad,
                DECODE (rcont.num_lote, 311, 511, 314, 514, 312, 512, 316, 516),
                'C', 0, 0, 0, vtiposaldo,
                abs(rcont.s107), abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                abs(rcont.s105),0,abs(rcont.s107),
                0, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, 0,
                0, abs(rcont.s117), abs(rcont.s118),
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0 );
    END;
    END LOOP;
  
    -- genera registro de Merchant para contracargos
    pqmonproc.inslog(vIdProc, 'M', 'Procesando registro de Merchant para contracargos.');
    FOR rCont IN ( SELECT COUNT(*) s107,
                          SUM(dm.incoming_dest_amount) s105,
                          SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'D', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)),0)) s117,--antes estaba en C
                          SUM(DECODE(SUBSTR(dm.INCOMING_P46TCUOT01,3,1),'C', TO_NUMBER(NVL(SUBSTR(dm.INCOMING_P46TCUOT01,4),0)),0)) s118,--antes estaba en D
                          doo.p29inlote num_lote,
                          doo.p48tipmov tipmov,
                          doo.cod_bcoemi cod_entidad ,
                          cp.cod_horacierre cod_hrcierre,
                          dm.incoming_dest_curren_code cod_moneda,
                          substr(doo.p48tiptra,1,2) p48tiptra      
                     FROM DISPUTAS_MASTER dm, 
                          DISPUTAS_OPERACION_ORIGINAL doo,
                          comercios_pmp cp
                    WHERE dm.Disputa_type   IN (15,16,17)
                      AND dm.cod_entadq     = pEntAdq
                      AND dm.incoming_date  = pFecSesion
                      AND dm.movdis_id      = doo.movdis_id
                      AND doo.cod_comercio  = cp.cod_comercio
                      AND doo.cod_bcoemi    IN (105,108)
                 GROUP BY doo.p29inlote, doo.p48tipmov , doo.cod_bcoemi, cp.cod_horacierre, dm.incoming_dest_curren_code , substr(doo.p48tiptra,1,2) ) 
    LOOP
    BEGIN
        vaux := '';
        IF pEntAdq = 'BM' THEN
            vaux := '98';
        ELSIF pEntAdq = 'BP' THEN
            vaux := '99';
        END IF;

        IF SUBSTR(rCont.p48tiptra,1,2) = '01' THEN
            vaux := '99'||vaux;
        ELSIF (SUBSTR(rCont.p48tiptra,1,2) = '06' OR SUBSTR(rCont.p48tiptra,1,2) = '10' )THEN
            vaux := '98'||vaux;
        ELSIF SUBSTR(rCont.p48tiptra,1,2) = '14' THEN
            vaux := '97'||vaux;
        ELSIF SUBSTR(rCont.p48tiptra,1,2) = '20' THEN
            vaux := '89'||vaux;
        END IF;

        vCod_entidadM := to_number(vaux) ;

        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaca_s106_accc, 0),        NVL(cuoabo_s109_accc, 0),
               NVL(cuocar_s110_accc, 0),        NVL(abomis_s113_accc, 0),
               NVL(carmis_s114_accc, 0),        NVL(rechazos_s116_accc, 0),
               NVL(cuochaca_s118_accc, 0),      NVL(cuorechazos_s119_accc, 0),
               NVL(rechcar_s120_accc, 0),       NVL(cuorechabo_s121_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),       NVL(rcarmis_s136_accc, 0),
               NVL(rabomis_s_s139_accc, 0),     NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs106,  vs109,  vs110,  vs113,
               vs114,   vs116,  vs118,  vs119,  vs120,  vs121,  vs123,  vs125,
               vs127,   vs128,  vs130,  vs132,  vS135,  vs136,  vs139,  vS140
          FROM clr_resumen_platco
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'M'
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = vcod_entidadm
           AND fec_sesion   = pfecsesion;

        vimp_neto_signo_comp :=  vs86  + vS87 - vS88 - vS89 - vS106 + abs(rCont.S105) + vS109 - vS110 + vS113 - vS114 + vS116 - abs(rCont.S117) + abs(rCont.S118) - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vS135 - vS136 + vS139 - vS140;

        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'D';
        ELSE
            vTipoSaldo := 'C';
        END IF;
        
        vimp_neto_signo_comp := vimp_neto_signo_comp * -1 ;

        UPDATE clr_resumen_platco
           SET ichaab_s105_accc     = abs(rcont.s105),
               nchaab_s107_accc     = rcont.s107,      
               cuochaab_s117_accc   = abs(rcont.s117),
               CUOCHACA_S118_ACCC   = abs(rcont.s118),   --Se agrego 21/11/2008
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_entadq   = pentadq
           AND cod_hrcierre = rcont.cod_hrcierre
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'M'
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = vcod_entidadm
           AND fec_sesion   = pfecsesion;
           
    EXCEPTION 
        WHEN no_data_found THEN
             vimp_neto_signo_comp :=  abs(rCont.S105) - abs(rCont.S117) + abs(rCont.S118);
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'D';
             ELSE
                vTipoSaldo := 'C';
             END IF;
             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote,
                cod_entidad, num_lote, tipo_saldo, num_oper, imp_neto,
                imp_neto_signo, tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc )
             VALUES 
                (pfecsesion, rcont.cod_hrcierre, rcont.cod_moneda, 'M',
                vcod_entidadm, rcont.num_lote, 'C', 0, 0,
                0, vtiposaldo, rcont.s107, abs(vimp_neto_signo_comp),
                -1 * vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                abs(rcont.s105), 0, rcont.s107,
                0, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, 0,
                0, abs(rcont.s117), abs(rcont.s118),
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0);
    END;
    END LOOP;

    COMMIT;

    pqmonproc.inslog(vIdProc, 'M', 'Termino Compensacion platco');

    /* FIN DE PROCESO
    *****************/
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');
    
EXCEPTION
    WHEN OTHERS THEN
         ROLLBACK;
         vOraErr:=SUBSTR(SQLERRM,1,200);         
         pqmonproc.inslog(vIdProc, 'E', vOraErr);
         vretc := pqctlproc.updctlproc(vIdProc, 'E');
         vretc := pqmonproc.updmonproc(vIdProc, 'E', '1');
         RAISE_APPLICATION_ERROR(-20000,vOraErr);         
END;


FUNCTION P_COMP_PLATCO_RETORNOS (pEntAdq CHAR, pCodHrCierre CHAR, pFecSesion DATE:=TRUNC(SYSDATE-1)) RETURN CHAR
IS
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure    : P_COMP_PLATCO_RETORNOS
-- Proposito    : Generar Compensacion Platco
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    vS105   NUMBER(16) := 0;
    vS106   NUMBER(16) := 0;
    vS107   NUMBER := 0;
    vS108   NUMBER := 0;

    vimp_neto_comp          NUMBER(16) := 0;
    vimp_neto_signo_comp    NUMBER(16) := 0;

    vS86    NUMBER(16) := 0;
    vS87    NUMBER(16) := 0;
    vS88    NUMBER(16) := 0;
    vS89    NUMBER(16) := 0;
    vS109   NUMBER(16) := 0;
    vS110   NUMBER(16) := 0;
    vS113   NUMBER(16) := 0;
    vS114   NUMBER(16) := 0;
    vS115   NUMBER(16) := 0;
    vS116   NUMBER(16) := 0;
    vS117   NUMBER(16) := 0;
    vS118   NUMBER(16) := 0;
    vS119   NUMBER(16) := 0;
    vS120   NUMBER(16) := 0;
    vS121   NUMBER(16) := 0;
    vS122   NUMBER(16) := 0;
    vS123   NUMBER(16) := 0;
    vS124   NUMBER(16) := 0;
    vS125   NUMBER(16) := 0;
    vS126   NUMBER(16) := 0;
    vS127   NUMBER(16) := 0;
    vS128   NUMBER(16) := 0;
    vS129   NUMBER(10) := 0;
    vS130   NUMBER(16) := 0;
    vS131   NUMBER(10) := 0;
    vS132   NUMBER(16) := 0;
    vS133   NUMBER(10) := 0;
    vS134   NUMBER(10) := 0;
    vS135   NUMBER(16) := 0;
    vS136   NUMBER(16) := 0;
    vS137   NUMBER(10) := 0;
    vS138   NUMBER(10) := 0;
    vS139   NUMBER(16) := 0;
    vS140   NUMBER(16) := 0;
    
    vAux            VARCHAR2(4);
    vCod_entidadM   NUMBER;
    vTipoSaldo      CHAR(1);

    vIdProc       NUMBER;
    vRetC         VARCHAR2(100);
    vOraCode      NUMBER;
    vOraErr       VARCHAR2(200);
    vErrMsg       VARCHAR2(200);
    vErrCod       VARCHAR2(2);

BEGIN

    /* ID DE PROCESO
    ****************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc('**',pFecSesion,'SES','PCOMPPLATCO');


    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIdProc, 'M', 'Inicia Compensacion platco retornos.');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre );

    -- Procesa retornos PRICE
    pqmonproc.inslog(vIdProc, 'M', 'Procesando retornos PRICE.');
    FOR rCont IN (SELECT SUM(f_GetNumero('S115',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S115,
                     SUM(f_GetNumero('S122',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S122,
                     SUM(f_GetNumero('S120_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S120_1,
                     SUM(f_GetNumero('S120_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S120_2,
                     SUM(f_GetNumero('S120_3',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S120_3,
                     SUM(f_GetNumero('S116_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S116_1,
                     SUM(f_GetNumero('S116_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S116_2,
                     SUM(f_GetNumero('S116_3',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S116_3,
                     SUM(f_GetNumero('S121_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S121_1,
                     SUM(f_GetNumero('S121_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S121_2,
                     SUM(f_GetNumero('S119_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S119_1,
                     SUM(f_GetNumero('S119_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S119_2,
                     rr.inlote_p29_actc num_lote,
                     rr.tipmov_p48_actc tipmov,
                     TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)) cod_entidad,
                     rr.montra_p49_actc cod_moneda,
                     cp.cod_horacierre cod_hrcierre
                FROM RECHAZOS_RETORNOS rr,
                     COMERCIOS_PMP cp
               WHERE rr.COD_ENTADQ      = pEntAdq
                 AND rr.FEC_SESION      = pFecSesion
                 AND rr.TIPO_INCOMING   = 'IL'   --(Restringue retornos solo de PLATCO)
                 AND TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)) in (105,108)
                 AND rtrim(rr.ideest_p42_actc) = cp.cod_comercio
                 AND rr.inlote_p29_actc = '511'
            GROUP BY rr.inlote_p29_actc, rr.tipmov_p48_actc , TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)), rr.montra_p49_actc, cp.cod_horacierre )
    LOOP
    BEGIN
        -- Calcula importe de cargo al emisor
        vS120 := rCont.S120_1 + rCont.S120_2 + rCont.S120_3;
        -- Calcula importe de abono al emisor
        vS116 := rCont.S116_1 + rCont.S116_2 + rCont.S116_3;
        -- Calcula importe de abono de comisiones al emisor
        vS121 := rCont.S121_1 + rCont.S121_2;
        -- Calcula importe de cargo de comisiones al emisor
        vS119 := rCont.S119_1 + rCont.S119_2;

        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(ichaca_s106_accc, 0),
               NVL(cuoabo_s109_accc, 0),        NVL(cuocar_s110_accc, 0),
               NVL(abomis_s113_accc, 0),        NVL(carmis_s114_accc, 0),
               NVL(cuochaab_s117_accc, 0),      NVL(cuochaca_s118_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),       NVL(rcarmis_s136_accc, 0),
               NVL(rabomis_s_s139_accc, 0),     NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs106,  vs109,  vs110,
               vs113,   vs114,  vs117,  vs118,  vs123,  vs125,  vs127,  vs128,
               vs130,   vs132,  vs135,  vs136,  vs139,  vs140
          FROM clr_resumen_platco
         WHERE cod_hrcierre     = rCont.cod_hrcierre
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = rcont.tipmov
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = rcont.cod_moneda
           AND tipo_lote        = 'E';

        vimp_neto_signo_comp :=  vS86  + vS87 - vS88 - vS89 + vS105 - vS106 + vS109 - vS110 + vS113 - vS114 + abs(vS116) - vS117 + vS118 - abs(vS119) - abs(vS120) + abs(vS121) - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vS135 - vS136 + vS139 - vS140;

        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;

        DBMS_OUTPUT.PUT_LINE('vimp_neto_signo_comp: '||vimp_neto_signo_comp);

        BEGIN

        UPDATE clr_resumen_platco
           SET nrechazos_s115_accc  = rcont.s115,
               rechazos_s116_accc   = abs(vs116),
               cuorechazos_s119_accc= abs(vs119),
               rechcar_s120_accc    = abs(vs120),
               cuorechabo_s121_accc = abs(vs121),
               nrechcar_s122_accc   = rcont.s122,
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_hrcierre = rcont.cod_hrcierre
           AND cod_entadq   = pentadq
           AND fec_sesion   = pFecSesion
           AND num_lote     = rcont.num_lote
           AND tipmov       = rcont.tipmov
           AND cod_entidad  = rcont.cod_entidad
           AND cod_moneda   = rcont.cod_moneda
           AND tipo_lote    = 'E';
       EXCEPTION
        WHEN OTHERS THEN
             vOraErr:=SUBSTR(SQLERRM,1,200);
             pqmonproc.inslog(vIdProc, 'E', vOraErr);
       END;

    EXCEPTION
        WHEN no_data_found THEN
             vimp_neto_signo_comp :=  abs(vS116) - abs(vS119) - abs(vS120) + abs(vS121);
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;

             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entidad,
                num_lote, tipo_saldo, num_oper, imp_neto, imp_neto_signo,
                tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES
                (pFecSesion, rcont.cod_hrcierre, rcont.cod_moneda, 'E', rcont.cod_entidad,
                rcont.num_lote, 'C', 0, 0, 0,
                vtiposaldo, rcont.s115, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, rcont.s115,
                abs(vs116), 0, 0,
                abs(vs119), abs(vs120), abs(vs121),
                rcont.s122, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0, 
                0, 0, 0);
    END;
    END LOOP;

    -- Procesa retornos PRICE
    pqmonproc.inslog(vIdProc, 'M', 'Procesando retornos PRICE - Miscelaneos');
    FOR rCont IN ( SELECT SUM (DECODE (rr.SIGCU1_P46_ACTC, 'D', 1, 0)) s133,
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'C', 1, 0)) s134, 
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'D', rr.IMPCU1_P46_ACTC, 0)) s135,
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'C', rr.IMPCU1_P46_ACTC, 0)) s136,                       
                          '511' num_lote, 
                          DECODE(COD_ENTADQ,'BM',105,'BP',108) cod_entidad
                     FROM rechazos_retornos rr
                    WHERE tipo_incoming     = 'IL'
                      AND idemen_p00_actc   = '1744'
                      AND estado = 'P'
                      AND rr.cod_entadq     = pEntAdq
                      AND rr.FEC_SESION = pFecSesion
                    GROUP BY COD_ENTADQ )
    LOOP
    BEGIN

        vS133 := rcont.s133;
        vS134 := rCont.s134;
        vS135 := rCont.s135;
        vS136 := rCont.s136;
        
        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(ichaca_s106_accc, 0),
               NVL(cuoabo_s109_accc, 0),        NVL(cuocar_s110_accc, 0),
               NVL(abomis_s113_accc, 0),        NVL(carmis_s114_accc, 0),
               NVL(rechazos_s116_accc,0),       NVL(cuorechazos_s119_accc, 0),
               NVL(cuochaab_s117_accc, 0),      NVL(cuochaca_s118_accc, 0),
               NVL(rechcar_s120_accc, 0),       NVL(cuorechabo_s121_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s_s139_accc, 0),     NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs106,  vs109,  vs110,
               vs113,   vs114,  vS116,  vs117,  vs118,  vS119,  vS120,  vS121,
               vs123,   vs125,  vs127,  vs128,  vs130,  vs132,  vS139,   vS140
          FROM clr_resumen_platco
         WHERE cod_hrcierre     = '1'
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = 'c'
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = '937'
           AND tipo_lote        = 'E';

        vimp_neto_signo_comp :=  vS86  + vS87 - vS88 - vS89 + vS105 - vS106 + vS109 - vS110 + vS113 - vS114 + vS116 - vS117 + vS118 - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + abs(vS135) - abs(vS136) + vS139 - vS140;
              
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;

        BEGIN

        UPDATE clr_resumen_platco
           SET nrabomis_s133_accc    = rcont.s133,
               nrcarmis_s134_accc    = rcont.s134,
               rabomis_s135_accc     = abs(rcont.s135),
               rcarmis_s136_accc     = abs(rcont.s136),
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_hrcierre = '1'
           AND cod_entadq   = pentadq
           AND fec_sesion   = pFecSesion
           AND num_lote     = rcont.num_lote
           AND tipmov       = 'c'
           AND cod_entidad  = rcont.cod_entidad
           AND cod_moneda   = '937'
           AND tipo_lote    = 'E';
           
       EXCEPTION
        WHEN OTHERS THEN
             vOraErr:=SUBSTR(SQLERRM,1,200);
             pqmonproc.inslog(vIdProc, 'E', vOraErr);
       END;

    EXCEPTION
        WHEN no_data_found THEN
             dbms_output.put_line('vS135: '||vS135); 
             dbms_output.put_line('vimp_neto_signo_comp:'||vimp_neto_signo_comp); 
                
             vimp_neto_signo_comp :=  abs(vS135) - abs(vS136);
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;

             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entidad,
                num_lote, tipo_saldo, num_oper, imp_neto, imp_neto_signo,
                tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES
                (pFecSesion, '1', '937', 'E', rcont.cod_entidad,
                rcont.num_lote, 'C', 0, 0, 0,
                vtiposaldo, 0, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, pentadq, 'c', 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, rcont.S133, rcont.S134, 
                abs(vS135),abs(vS136),0,
                0, 0, 0);
    END;
    END LOOP;
    
    -- Procesa retornos PRICE SOLUCIONADOS
    pqmonproc.inslog(vIdProc, 'M', 'Procesando Retornos Price solucionados.');
    FOR rCont IN ( SELECT SUM(f_GetNumero('S115',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S124,
                          SUM(f_GetNumero('S122',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S126,
                          SUM(f_GetNumero('S120_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S125_1,
                          SUM(f_GetNumero('S120_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S125_2,
                          SUM(f_GetNumero('S120_3',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S125_3,
                          SUM(f_GetNumero('S116_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S123_1,
                          SUM(f_GetNumero('S116_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S123_2,
                          SUM(f_GetNumero('S116_3',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S123_3,
                          SUM(f_GetNumero('S121_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S127_1,
                          SUM(f_GetNumero('S121_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S127_2,
                          SUM(f_GetNumero('S119_1',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S128_1,
                          SUM(f_GetNumero('S119_2',IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC,SIGCU1_P46_ACTC,IMPCU1_P46_ACTC)) S128_2,
                          rr.inlote_p29_actc num_lote,
                          rr.tipmov_p48_actc tipmov,
                          TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)) cod_entidad,
                          rr.montra_p49_actc cod_moneda,
                          cp.cod_horacierre cod_hrcierre
                     FROM RECHAZOS_RETORNOS rr,
                          COMERCIOS_PMP cp
                    WHERE rr.COD_ENTADQ      = pEntAdq
                      AND rr.FEC_UPD         = pFecSesion   --La fecha es la de actualizacion del registro a 'T'
                      AND rr.TIPO_INCOMING   = 'IL'   --(Restringue retornos solo de PLATCO)
                      AND rr.ESTADO          = 'T'    --(Solo muestra los solucionados)
                      AND TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)) in (105,108)
                      AND rtrim(rr.ideest_p42_actc) = cp.cod_comercio
                      AND rr.inlote_p29_actc = '511'
                 GROUP BY rr.inlote_p29_actc, rr.tipmov_p48_actc , TO_NUMBER(SUBSTR(rr.ideadq_p32_actc,3,4)), rr.montra_p49_actc, cp.cod_horacierre )
    LOOP
    BEGIN
        -- Calcula importe de cargo al emisor
        vS123 := rCont.S123_1 + rCont.S123_2 + rCont.S123_3;
        -- Calcula importe de abono al emisor
        vS125 := rCont.S125_1 + rCont.S125_2 + rCont.S125_3;
        -- Calcula importe de cargo de comisiones al emisor
        vS127 := rCont.S127_1 + rCont.S127_2;
        -- Calcula importe de abono de comisiones al emisor
        vS128 := rCont.S128_1 + rCont.S128_2;

        SELECT NVL(impabo_s86_accc, 0),     NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),     NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),    NVL(ichaca_s106_accc, 0),
               NVL(cuoabo_s109_accc, 0),    NVL(cuocar_s110_accc, 0),
               NVL(abomis_s113_accc, 0),    NVL(carmis_s114_accc, 0),
               NVL(rechazos_s116_accc, 0),  NVL(cuochaab_s117_accc, 0),
               NVL(cuochaca_s118_accc, 0),  NVL(cuorechazos_s119_accc, 0),
               NVL(rechcar_s120_accc, 0),   NVL(cuorechabo_s121_accc, 0),
               NVL(repabo_s130_accc, 0),    NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),   NVL(rcarmis_s136_accc, 0),
               NVL(rabomis_s_s139_accc, 0), NVL(rcarmis_s_s140_accc, 0)
          INTO vs86,   vs87,   vs88,   vs89,   vs105,  vs106,  vs109,  vs110,
               vs113,  vs114,  vs116,  vs117,  vs118,  vs119,  vs120,  vs121,
               vs130,  vs132,  vS135,  vS136,  vS139,  vS140       
          FROM clr_resumen_platco
         WHERE cod_hrcierre     = rcont.cod_hrcierre
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = rcont.tipmov
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = rcont.cod_moneda
           AND tipo_lote        = 'E';

        vimp_neto_signo_comp :=  vS86  + vS87 - vS88 - vS89 + vS105 - vS106 + vS109 - vS110 + vS113 - vS114 + vS116 - vS117 + vS118 - vS119 - vS120 + vS121 - abs(vS123) + abs(vS125) - abs(vS127) + abs(vS128) + vS130 - vS132 + vS135 - vS136 + vS139 - vS140;

        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;

        UPDATE clr_resumen_platco
           SET rechcar_s_s123_accc      = abs(vs123),
               nrechcar_s_s124_accc     = rcont.s124,
               rechazos_s_s125_accc     = abs(vs125),
               nrechazos_s_s126_accc    = rcont.s126,
               cuorechazos_s_s127_accc  = abs(vs127),
               cuorechabo_s_s128_accc   = abs(vs128),
               imp_neto_comp            = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp      = vimp_neto_signo_comp,
               tipo_saldo_comp          = vtiposaldo
         WHERE cod_hrcierre     = rcont.cod_hrcierre
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = rcont.tipmov
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = rcont.cod_moneda
           AND tipo_lote        = 'E';

    EXCEPTION
        WHEN no_data_found THEN
             vimp_neto_signo_comp :=  abs(vS125) - abs(vS123) - abs(vS127) + abs(vS128);

             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;

             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entidad,
                num_lote, tipo_saldo, num_oper, imp_neto, imp_neto_signo,
                tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES
                (pFecSesion, rcont.cod_hrcierre, rcont.cod_moneda, 'E', rcont.cod_entidad,
                rcont.num_lote, 'C', 0, 0, 0,
                vtiposaldo, rcont.s124, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, pentadq, rcont.tipmov, 0,
                0, 0, 0,
                0, 0, 0,
                0, abs(vs123), rcont.s124,
                abs(vs125), rcont.s126,
                abs(vs127), abs(vs128),
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0 );
    END;
    END LOOP;
    
   -- Procesa retornos Solucionados PRICE
    pqmonproc.inslog(vIdProc, 'M', 'Procesando Retornos Solucionados PRICE - Miscelaneos');
    FOR rCont IN ( SELECT SUM (DECODE (rr.SIGCU1_P46_ACTC, 'D', 1, 0)) s138,
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'C', 1, 0)) s137, 
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'D', rr.IMPCU1_P46_ACTC, 0)) s140,
                          SUM (DECODE (rr.SIGCU1_P46_ACTC, 'C', rr.IMPCU1_P46_ACTC, 0)) s139,                       
                          '511' num_lote, 
                          DECODE(COD_ENTADQ,'BM',105,'BP',108) cod_entidad
                     FROM rechazos_retornos rr
                    WHERE tipo_incoming     = 'IL'
                      AND idemen_p00_actc   = '1744'
                      AND estado = 'T'
                      AND rr.cod_entadq     = pEntAdq
                      AND rr.FEC_UPD = pFecSesion
                    GROUP BY COD_ENTADQ )
    LOOP
    BEGIN

        vS137 := rcont.s137;
        vS138 := rCont.s138;
        vS139 := rCont.s139;
        vS140 := rCont.s140;
        
        SELECT NVL(impabo_s86_accc, 0),         NVL(impana_s87_accc, 0),
               NVL(impcar_s88_accc, 0),         NVL(impanc_s89_accc, 0),
               NVL(ichaab_s105_accc, 0),        NVL(ichaca_s106_accc, 0),
               NVL(cuoabo_s109_accc, 0),        NVL(cuocar_s110_accc, 0),
               NVL(abomis_s113_accc, 0),        NVL(carmis_s114_accc, 0),
               NVL(rechazos_s116_accc,0),       NVL(cuorechazos_s119_accc, 0),
               NVL(cuochaab_s117_accc, 0),      NVL(cuochaca_s118_accc, 0),
               NVL(rechcar_s120_accc, 0),       NVL(cuorechabo_s121_accc, 0),
               NVL(rechcar_s_s123_accc, 0),     NVL(rechazos_s_s125_accc, 0),
               NVL(cuorechazos_s_s127_accc, 0), NVL(cuorechabo_s_s128_accc, 0),
               NVL(repabo_s130_accc, 0),        NVL(repcar_s132_accc, 0),
               NVL(rabomis_s135_accc, 0),       NVL(rcarmis_s136_accc, 0)
          INTO vs86,    vs87,   vs88,   vs89,   vs105,  vs106,  vs109,  vs110,
               vs113,   vs114,  vS116,  vs117,  vs118,  vS119,  vS120,  vS121,
               vs123,   vs125,  vs127,  vs128,  vs130,  vs132,  vS135,   vS136
          FROM clr_resumen_platco
         WHERE cod_hrcierre     = '1'
           AND cod_entadq       = pentadq
           AND fec_sesion       = pFecSesion
           AND num_lote         = rcont.num_lote
           AND tipmov           = 'c'
           AND cod_entidad      = rcont.cod_entidad
           AND cod_moneda       = '937'
           AND tipo_lote        = 'E';

        vimp_neto_signo_comp :=  vS86  + vS87 - vS88 - vS89 + vS105 - vS106 + vS109 - vS110 + vS113 - vS114 + vS116 - vS117 + vS118 - vS119 - vS120 + vS121 - vS123 + vS125 - vS127 + vS128 + vS130 - vS132 + vS135 - vS136 + abs(vS139) - abs(vS140);
         
        IF vimp_neto_signo_comp >= 0 THEN
            vTipoSaldo := 'C';
        ELSE
            vTipoSaldo := 'D';
        END IF;

        BEGIN

        UPDATE clr_resumen_platco
           SET nrabomis_s_s137_accc    = rcont.s137,
               nrcarmis_s_s138_accc    = rcont.s138,
               rabomis_s_s139_accc     = abs(rcont.s139),
               rcarmis_s_s140_accc     = abs(rcont.s140),
               imp_neto_comp        = abs(vimp_neto_signo_comp),
               imp_neto_signo_comp  = vimp_neto_signo_comp,
               tipo_saldo_comp      = vtiposaldo
         WHERE cod_hrcierre = '1'
           AND cod_entadq   = pentadq
           AND fec_sesion   = pFecSesion
           AND num_lote     = rcont.num_lote
           AND tipmov       = 'c'
           AND cod_entidad  = rcont.cod_entidad
           AND cod_moneda   = '937'
           AND tipo_lote    = 'E';
           
       EXCEPTION
        WHEN OTHERS THEN
             vOraErr:=SUBSTR(SQLERRM,1,200);
             pqmonproc.inslog(vIdProc, 'E', vOraErr);
       END;

    EXCEPTION
        WHEN no_data_found THEN
             vimp_neto_signo_comp := abs(vS139) - abs(vS140);
             IF vimp_neto_signo_comp >= 0 THEN
                vTipoSaldo := 'C';
             ELSE
                vTipoSaldo := 'D';
             END IF;

             INSERT INTO clr_resumen_platco
                (fec_sesion, cod_hrcierre, cod_moneda, tipo_lote, cod_entidad,
                num_lote, tipo_saldo, num_oper, imp_neto, imp_neto_signo,
                tipo_saldo_comp, num_oper_comp, imp_neto_comp,
                imp_neto_signo_comp, numabo_s74_accc, numana_s75_accc,
                numcar_s76_accc, numanc_s77_accc, impabo_s86_accc,
                impana_s87_accc, impcar_s88_accc, impanc_s89_accc,
                ichaab_s105_accc, ichaca_s106_accc, nchaab_s107_accc,
                nchaca_s108_accc, cuoabo_s109_accc, cuocar_s110_accc,
                nabomis_s111_accc, ncarmis_s112_accc, abomis_s113_accc,
                carmis_s114_accc, cod_entadq, tipmov, nrechazos_s115_accc,
                rechazos_s116_accc, cuochaab_s117_accc, cuochaca_s118_accc,
                cuorechazos_s119_accc, rechcar_s120_accc, cuorechabo_s121_accc,
                nrechcar_s122_accc, rechcar_s_s123_accc, nrechcar_s_s124_accc,
                rechazos_s_s125_accc, nrechazos_s_s126_accc,
                cuorechazos_s_s127_accc, cuorechabo_s_s128_accc,
                nrepabo_s129_accc, repabo_s130_accc, nrepcar_s131_accc,
                repcar_s132_accc, nrabomis_s133_accc, nrcarmis_s134_accc,
                rabomis_s135_accc, rcarmis_s136_accc, nrabomis_s_s137_accc,
                nrcarmis_s_s138_accc, rabomis_s_s139_accc, rcarmis_s_s140_accc)
             VALUES
                (pFecSesion, '1', '937', 'E', rcont.cod_entidad,
                rcont.num_lote, 'C', 0, 0, 0,
                vtiposaldo, 0, abs(vimp_neto_signo_comp),
                vimp_neto_signo_comp, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, pentadq, 'c', 0,
                0, 0, 0,
                0, 0, 0,
                0, 0, 0,
                0, 0,
                0, 0,
                0, 0, 0,
                0, 0, 0,
                0,0,rcont.S137,
                rcont.S138, abs(vS139), abs(vS140));
    END;
    END LOOP;


    COMMIT;

    pqmonproc.inslog(vIdProc, 'M', 'Termino Compensacion platco Retornos');

    /* FIN DE PROCESO
    *****************/
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');
    RETURN '0~';

EXCEPTION
    WHEN OTHERS THEN
         ROLLBACK;
         vOraErr:=SUBSTR(SQLERRM,1,200);
         pqmonproc.inslog(vIdProc, 'E', vOraErr);
         vretc := pqctlproc.updctlproc(vIdProc, 'E');
         vretc := pqmonproc.updmonproc(vIdProc, 'E', '1');
         RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION F_CLR_GENDATRPT_PCLM_HORA (pCodEntAdq CHAR, pFecSesion DATE, pCodHrCierre CHAR,phra_proceso CHAR) RETURN CHAR
IS

vTXNNumTxns CLR_RESUMEN_PCLM.TXN_NUMTXNS%TYPE:=0;
vTXNImpNeto CLR_RESUMEN_PCLM.TXN_IMPNETO%TYPE:=0;
vSGCNumTxns CLR_RESUMEN_PCLM.SGC_NUMTXNS%TYPE:=0;
vSGCImpNeto CLR_RESUMEN_PCLM.SGC_IMPNETO%TYPE:=0;
vFlgNeg     BOOLEAN;
vSigno      CHAR(1);
vMonto      NUMBER:=0;
vComiEMI    NUMBER:=0;
vMoneda     CHAR(3);
vOraCode    NUMBER;

BEGIN

  -- Elimina la Informacion Anterior
  DELETE CLR_RESUMEN_PCLM_HORA
   WHERE COD_ENTADQ = pCodEntAdq
     AND FEC_SESION = pFecSesion
     AND COD_HRCIERRE = pCodHrCierre
     AND HRA_PROCESO  = phra_proceso;
  COMMIT;

  -- Codigo de Moneda
  vMoneda:=PQCOMERCIOS.GCW_F_GETMONEDAVIG(pFecSesion);
  vTXNNumTxns:= 0;
  vTXNImpNeto:= 0;
  -- Informacion del Resumen de Clearing
  SELECT SUM(NUM_OPER - NVL(nabomis_s111_accc,0) -  NVL(ncarmis_s112_accc,0)),
         SUM(IMP_NETO_SIGNO + NVL(abomis_s113_accc,0) - NVL(carmis_s114_accc,0) )
    INTO vTXNNumTxns,
         vTXNImpNeto
    FROM CLR_RESUMEN_HORA
   WHERE COD_ENTADQ = pCodEntAdq
     AND FEC_SESION = TRUNC(pFecSesion)
     AND COD_HRCIERRE = pCodHrCierre
     AND HRA_PROCESO  = phra_proceso
     AND COD_MONEDA = vMoneda
     AND TIPO_LOTE = 'M'
     AND COD_ENTIDAD NOT IN (105,108); -- JPC: no considera representaciones

  -- Informacion de la Interfaz PLIQ
  FOR r IN (SELECT P00IDMSG,
                   P03CODPRO,
                   P04IMPTRA,
                   P46TCUOT01,
                   P46TCUOT02,
                   PQPCLR_HORA.f_findprov(NULL, NULL, NULL, P4899FILLER) NUMTRXPROV, -- NUMERO DE TRX PROVIMILLAS CREADOS
                   PQPCLR_HORA.f_findcash(NULL, P4899FILLER) NUMTRXCASH  -- NUMERO DE TRX CASHBACK CREADOS
              FROM TMP_PCLM_ACTC
             WHERE P28SESION = TO_CHAR(pFecSesion,'RRMMDD')
               AND COD_HRCIERRE = pCodHrCierre
               AND P49MONTRA = vMoneda
               AND P00IDMSG NOT IN (1144,1442,1744)
               AND NOT (P00IDMSG = 1444 AND P31REFADQ IS NULL) -- JPC 20071030
               AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                       WHERE FEC_SESION = pFecSesion
                       AND COD_ARCHIVO like 'MX%'
                       AND HRA_PROCESO = phra_proceso)
               AND SUBSTR(P32IDADQ,3,4) IN (SELECT C00LCSB
                                              FROM ENTIDADES_PRICE
                                             WHERE COD_ENTADQ = pCodEntAdq
                                               AND C00LTIPO = 0)) LOOP

      -- Inicializacion
      vFlgNeg:=FALSE;
    --vSGCNumTxns:=vSGCNumTxns + 1;
    --vSGCNumTxns:=vSGCNumTxns + 1 + r.NUMTRXPROV;
      vSGCNumTxns:=vSGCNumTxns + 1 + r.NUMTRXPROV + r.NUMTRXCASH; -- JMG 16/07/2013: TRX CASHBACK
      vComiEMI:=0;

      -- Comision Emisor
      IF r.P46TCUOT01 IS NOT NULL THEN
         vSigno:=SUBSTR(r.P46TCUOT01,3,1);
         vMonto:=SUBSTR(r.P46TCUOT01,4,8);
         IF vSigno = 'D' THEN -- actua como cargo
            vComiEMI:=vComiEMI-vMonto;
         ELSIF vSigno = 'C' THEN -- actua como abono
               vComiEMI:=vComiEMI+vMonto;
         END IF;
      END IF;
      IF r.P46TCUOT02 IS NOT NULL THEN
         vSigno:=SUBSTR(r.P46TCUOT02,3,1);
         vMonto:=SUBSTR(r.P46TCUOT02,4,8);
         IF vSigno='D' THEN
            vComiEMI:=vComiEMI-vMonto;
         ELSIF vSigno='C' THEN
               vComiEMI:=vComiEMI+vMonto;
         END IF;
      END IF;

      -- Determina Importes en Negativo
      IF r.P00IDMSG IN (1444,1442) THEN
         vFlgNeg:=TRUE;
      ELSIF r.P00IDMSG = 1244 AND (r.P03CODPRO >= 200000 AND r.P03CODPRO <= 290000) THEN
            vFlgNeg:=TRUE;
      END IF;
      IF vFlgNeg THEN
         r.P04IMPTRA:= (-1) * r.P04IMPTRA;
      END IF;

      -- Importe Neto
      vSGCImpNeto:=vSGCImpNeto + (r.P04IMPTRA - vComiEMI);

  END LOOP;

  -- Registra los Datos en la Tabla de Resumen
  INSERT INTO CLR_RESUMEN_PCLM_HORA (COD_ENTADQ,
                                FEC_SESION,
                                COD_HRCIERRE,
                                HRA_PROCESO,
                                TXN_NUMTXNS,
                                TXN_IMPNETO,
                                SGC_NUMTXNS,
                                SGC_IMPNETO)
                        VALUES (pCodEntAdq,
                                pFecSesion,
                                pCodHrCierre,
                                phra_proceso,
                                vTXNNumTxns,
                                vTXNImpNeto,
                                vSGCNumTxns,
                                vSGCImpNeto);

  COMMIT;

  -- Cuadre
  IF vTXNNumTxns <> vSGCNumTxns OR vTXNImpNeto <> vSGCImpNeto THEN
     RETURN '1';
  ELSE
     RETURN '0';
  END IF;

EXCEPTION
  WHEN OTHERS THEN
       vOraCode:=abs(SQLCODE);
       ROLLBACK;
       RETURN 'E|Error de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')';
END;

END;
/


GRANT EXECUTE ON SGCVNZ.PQPCLRRPT TO ROLE_SOPTECN;

