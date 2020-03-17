CREATE OR REPLACE PACKAGE SGCVNZ.PQPMAESTRO IS
/***************************************************************************************************
*********   Proyecto : Procesadora Venezuela   *****************************************************
*********   Proposito: Compensación y Conciliación Maestro    **************************************
 Historial
 =========
 Persona                  Fecha           Comentarios
 --------------------     -----------     ------------
 Jesús Muñante Gonzales.  15/Febrero/2008   Compensación y Conciliación Maestro

 parametro del Package: pbanco char [BM:Mercantil, BP:Provincial]
 parametro del Package: pfecha char [año|mes|dia 20070827]
 parametro del Package: pcarga char [N:Nuevo, R:Reproceso]
 Barbara Rivas.           03/02/2020        Compensaci�n y Conciliaci�n Maestro Naiguata 
 Francisco vasquez        04/03/2020        Ajustes producci�n
****************************************************************************************************/
vIdPart          CHAR           :=NULL;  /* INCLUSIÓN DEL PARTICIONAMINTO */
vidMes          NUMBER          :=NULL;
type Reg_TMP is record(
        NBR                            NUMBER,
        MSGTYPE                        CHAR(4),
        P02NUMTAR                      VARCHAR2(19),
        P03CODPRO                      NUMBER(6,0),
        P04IMPTRA                      NUMBER(12,0),
        P04IMPTRA_2                    NUMBER(12,0),
        P05IMPCON                      NUMBER(12,0),
        P09CONCON                      NUMBER(8,0),
        P11IDETRA                      NUMBER(6,0),
        P11IDETRA_2                    NUMBER(6,0),
        P12TIMLOC                      CHAR(14),
        P12TIMLOC_DATE                 CHAR(6),
        P12TIMLOC_TIME                 CHAR(6),
        P18LACTI                       NUMBER(4,0),
        P22PUNSER                      CHAR(12),
        P28SESION                      CHAR(8),
        P32IDADQ                       VARCHAR2(11),
        P32IDADQ_NBR                   CHAR(10),
        P37DATREF                      CHAR(12),
        P38NUMAUT                      CHAR(6),
        P39CODACR                      NUMBER(3,0),
        P41CSERI                       CHAR(10),
        P46TCUOT01                     VARCHAR2(11),
        P46TCUOT01_NBR                 CHAR(8),
        P46TCUOT01_SGN                 CHAR(1),
        P49MONTRA                      NUMBER(3,0),
        P50MONCON                      NUMBER(3,0));
type Reg_CLR is record(
        fec_sesion                     DATE,
        cod_hrcierre                   CHAR(1),
        cod_moneda                     CHAR(3),
        tipo_lote                      CHAR(1),
        cod_entidad                    NUMBER,
        tipo_saldo                     CHAR(1),
        num_lote                       VARCHAR2(3),
        num_oper                       NUMBER,
        imp_neto                       NUMBER,
        imp_neto_signo                 NUMBER,
        numabo_s74_accc                NUMBER(10,0),
        numana_s75_accc                NUMBER(10,0),
        numcar_s76_accc                NUMBER(10,0),
        numanc_s77_accc                NUMBER(10,0),
        impabo_s86_accc                NUMBER(16,0),
        impana_s87_accc                NUMBER(16,0),
        impcar_s88_accc                NUMBER(16,0),
        impanc_s89_accc                NUMBER(16,0),
        ichaab_s105_accc               NUMBER(16,0),
        ichaca_s106_accc               NUMBER(16,0),
        nchaab_s107_accc               NUMBER(10,0),
        nchaca_s108_accc               NUMBER(10,0),
        cuoabo_s109_accc               NUMBER(16,0),
        cuocar_s110_accc               NUMBER(16,0),
        nabomis_s111_accc              NUMBER(10,0),
        ncarmis_s112_accc              NUMBER(10,0),
        abomis_s113_accc               NUMBER(16,0),
        carmis_s114_accc               NUMBER(16,0),
        cod_entadq                     CHAR(2),
        tipmov                         CHAR(1),
        nrechazos_s115_accc            NUMBER(10,0),
        rechazos_s116_accc             NUMBER(16,0),
        cuochaab_s117_accc             NUMBER(16,0),
        cuochaca_s118_accc             NUMBER(16,0),
        cuorechazos_s119_accc          NUMBER(16,0),
        imp_neto_comp                  NUMBER,
        imp_neto_signo_comp            NUMBER,
        num_oper_comp                  NUMBER,
        tipo_saldo_comp                CHAR(1));

--Modificacion (2011/11 CPU - IPR 1017):
type Reg_TMP_COMP is record(
        cod_moneda                     CHAR(3),
        impana_s87_accc                NUMBER(16,0),
        numana_s75_accc                NUMBER(10,0),
        impcar_s88_accc                NUMBER(16,0),
        numcar_s76_accc                NUMBER(10,0),
        ichaab_s105_accc               NUMBER(16,0),
        nchaab_s107_accc               NUMBER(10,0),
        ichaca_s106_accc               NUMBER(16,0),
        nchaca_s108_accc               NUMBER(10,0),
        cuocar_s110_accc               NUMBER(16,0),
        cuoabo_s109_accc               NUMBER(16,0),
        imp_neto                       NUMBER,
        tipo_saldo                     CHAR(1));
--

  FUNCTION f_GetBuscaNumRegistros(preg Reg_TMP)return number;
  FUNCTION f_GetIdEntAdq( pcodentadq_nbr VARCHAR2 ) return VARCHAR2;
  FUNCTION f_GetImporteComisionP46(
                    pimportesgn     VARCHAR2,
                    pimportenbr     VARCHAR2,
                    pentadqnbr      VARCHAR2,
                    pcodmarca       VARCHAR2,
                    pcodmoncomis    NUMBER,
                    pcodmontrans    NUMBER,
                    pfecha          DATE ) RETURN VARCHAR2;
  FUNCTION f_GetTipoDiferencia(preg Reg_TMP) return char;
  FUNCTION f_ini_RegCLR return Reg_CLR;
  PROCEDURE p_CopyMovimientosMCP(pfecha DATE, pbanco VARCHAR2);
  PROCEDURE p_NoExisteArchivo(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2, pcod_proc VARCHAR2);
  PROCEDURE p_SaveConciliacion(preg Reg_TMP, pfecha DATE);
  PROCEDURE p_SaveDiferencia( preg Reg_TMP, pfecha DATE, ptipodiff VARCHAR2);
  FUNCTION pf_Load_compensacion(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2) RETURN VARCHAR2;
  FUNCTION pf_Load_conciliacion(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2:=NULL) RETURN VARCHAR2;

  FUNCTION pf_Load_compensacion_ngta(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2) RETURN VARCHAR2;
  FUNCTION pf_Load_conciliacion_ngta(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2:=NULL) RETURN VARCHAR2;
  PROCEDURE p_CopyMovimientosMCP_Ngta(pfecha DATE, pbanco VARCHAR2);
  PROCEDURE p_SaveDiferencia_Ngta( preg Reg_TMP, pfecha DATE, ptipodiff VARCHAR2);
  FUNCTION f_GetBuscaNumRegistros_Ngta(preg Reg_TMP) return number;
  FUNCTION f_GetTipoDiferencia_Ngta(preg Reg_TMP) return char;
  PROCEDURE p_SaveConciliacion_Ngta(preg Reg_TMP, pfecha DATE);
  
end;
/

GRANT EXECUTE ON SGCVNZ.PQPMAESTRO TO ROLE_SOPTECN;
CREATE OR REPLACE PACKAGE BODY SGCVNZ.PQPMAESTRO is
vIDFile         utl_file.file_type;
vDirTMP         varchar2(100)   := 'DIR-TMP';
vEntorno        varchar2(4)     := STD.F_GETVALPAR('ENTORNO');
vFile           varchar2(50);
vIdProc         number;
vErrMsg         varchar2(200);
vRetC           varchar2(100);
vErrCod         varchar2(2);
vOraCode        number;
vOraErr         varchar2(200);
eFinError       exception;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Funcion     : f_ini_RegTMP2
-- Autor       : Jesus Munante Gonzales
-- Descripción : Funcion que inicializa registros tipo Reg_TMP2.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION f_ini_RegCLR return Reg_CLR is
rCLR  Reg_CLR;
BEGIN
    return rCLR;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : p_SaveDiferencia
-- Autor       : Jesus Munante Gonzales
-- Descripción : proceso inserta el registro en la tabla de diferencias.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROCEDURE p_SaveDiferencia( preg Reg_TMP, pfecha DATE, ptipodiff VARCHAR2) AS
BEGIN
    INSERT INTO mcp_maestro_diff
            (p28sesion, cod_entadq, p02numtar, p03codpro,
             p04imptra, p05impcon, p09concon, p11idetra,
             p12timloc, p18lacti, p22punser, p32idadq,
             p37datref, p38numaut, p39codacr, p41cseri,
             p46tcuot01, p49montra, p50moncon, tipo_diff,
             fec_sesion, fec_concil, msgtype, p04imptra_2
            )
     VALUES (preg.p28sesion, preg.p32idadq, preg.p02numtar, preg.p03codpro,
             preg.p04imptra, preg.p05impcon, preg.p09concon, preg.p11idetra,
             preg.p12timloc, preg.p18lacti, preg.p22punser, preg.p32idadq,
             preg.p37datref, preg.p38numaut, preg.p39codacr, preg.p41cseri,
             preg.p46tcuot01, preg.p49montra, preg.p50moncon, ptipodiff,
             TO_DATE (preg.p28sesion, 'YYYYMMDD'), pfecha, preg.msgtype, preg.p04imptra_2
            );
COMMIT ;

END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : p_CopyMovimientosMCP
-- Autor       : Jesus Munante Gonzales
-- Descripción : proceso que actualiza la tabla de MCP_MAESTRO_CONC obteniendo
--               los datos de MCP_BP y MCP_BP.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROCEDURE p_CopyMovimientosMCP(pfecha DATE, pbanco VARCHAR2) AS
    vcant       PLS_INTEGER := 0;
    vest_proc   CHAR        := NULL;
    vfecha      DATE;
    wkIdPart   CHAR;                         /* VARIABLE  LA PARTICION */
    wkIdMes    CHAR(2);                   /*  VARIABLE SUBPARTICION */
    
BEGIN
    -- ACTUALIZA TABLA MAESTRO CONCILIACION
    FOR ndias IN REVERSE 0..5 LOOP
        vfecha := pfecha - ndias;
        wkIdPart   :=SGCVNZ.FN_GETPARTICION(TO_CHAR(vfecha -1,'YYYY'));  /* OBTENER LA PARTICION , fec_sesion = vfecha -1*/
        wkIdMes   :=TO_CHAR(TO_NUMBER(TO_CHAR(vfecha - 1,'MM')));                   /*  OBTENER LA SUBPARTICION, vfecha -1  */
        -- VERIFICA DIA PROCESADO
        BEGIN
           SELECT est_proc
             INTO vest_proc
             FROM ctl_procesos
            WHERE cod_entadq = pbanco
              AND fecha = vfecha
              AND cod_proc = 'PCCLMA';
        EXCEPTION
           WHEN OTHERS THEN
                vest_proc := 'X';
        END;
        -- VERIFICA ESTADO FINALIZADO
        IF NVL(vest_proc,'X') <> 'F' THEN
           SELECT COUNT (*)
             INTO vcant
             FROM mcp_maestro_conc
            WHERE cod_entadq = pbanco AND p28sesion = TO_CHAR(vfecha-1,'YYYYMMDD');
        END IF;
        -- COPY DATA
        IF NVL(vest_proc,'X') <> 'F' AND vcant = 0 THEN
            -- BANCO MERCANTIL
            IF pbanco='BM' THEN
                INSERT /*+ APPEND */INTO mcp_maestro_conc
                (id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, flg_concil, fec_sesion)
                SELECT /*+ INDEX(mcp idxp_mcp_bm_adqp28hc) */ 
                id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, 'P', TO_DATE(p28sesion,'YYYYMMDD')
                FROM mcp_bm mcp
                WHERE idparticion =wkIdPart
                AND    id_mes       =wkIdMes 
                AND cod_entadq = 'BM'
                AND p28sesion = TO_CHAR(vfecha-1,'YYYYMMDD')
                AND p48tiptra LIKE '10%'
                AND p71nummen LIKE '8010%';
            -- BANCO PROVINCIAL
            ELSIF pbanco='BP' THEN
                INSERT /*+ APPEND */INTO mcp_maestro_conc
                (id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, flg_concil, fec_sesion)
                SELECT /*+ INDEX(mcp idxp_mcp_bp_adqp28hc) */ 
                id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, 'P', TO_DATE(p28sesion,'YYYYMMDD')
                FROM mcp_bp mcp
                WHERE idparticion =wkIdPart
                AND    id_mes       =wkIdMes  
                AND cod_entadq = 'BP'
                AND p28sesion       = TO_CHAR(vfecha-1,'YYYYMMDD')
                AND p48tiptra LIKE '10%'
                AND p71nummen LIKE '8010%';
            END IF;
            COMMIT;
        END IF;
    END LOOP;

END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : p_SaveConciliacion
-- Autor       : Jesus Munante Gonzales
-- Descripción : proceso que actualiza el estado de la tabla de MCP_MAESTRO_CONC.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROCEDURE p_SaveConciliacion(preg Reg_TMP, pfecha DATE) AS
BEGIN
    -- ACTUALIZA ESTADO EN TABLA MAESTRO CONCILIACION

    IF (preg.msgtype = 'FREC') THEN
        -- VENTA
        UPDATE mcp_maestro_conc mcp
           SET flg_concil = 'C',
               fec_concil = pfecha,
               p03codpro_maestro = preg.p03codpro,
               p04imptra_maestro = preg.p04imptra,
               p46tcuot01_maestro = preg.p46tcuot01
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF (preg.msgtype = 'EREC') THEN
        -- ANULACION
        UPDATE mcp_maestro_conc mcp
           SET flg_concil = 'C',
               fec_concil = pfecha,
               p03codpro_maestro = preg.p03codpro,
               p04imptra_maestro = preg.p04imptra,
               p46tcuot01_maestro = preg.p46tcuot01
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;
    COMMIT;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Funcion     : f_GetBuscaNumRegistros
-- Autor       : Jesus Munante Gonzales
-- Descripción : Funcion que obtiene la cantidad de registros en la tabla
--               "MCP_MAESTRO_CONC" de a acuerdo al registro buscado.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION f_GetBuscaNumRegistros(preg Reg_TMP)return number is
    vncont  NUMBER(7);
BEGIN
    IF (preg.msgtype = 'FREC') THEN
        -- VENTAS
        SELECT NVL (COUNT (*), 0)
          INTO vncont
          FROM mcp_maestro_conc mcp
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF (preg.msgtype = 'EREC') THEN
        -- ANULACIONES
        SELECT NVL (COUNT (*), 0)
          INTO vncont
          FROM mcp_maestro_conc mcp
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;
    RETURN vncont;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Funcion     : f_GetIdEntAdq
-- Autor       : Jesus Munante Gonzales
-- Descripción : Retorna La Entidad correspondiente a los codigos enviados por
--               Maestro.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION f_GetIdEntAdq( pcodentadq_nbr VARCHAR2 ) return VARCHAR2 is
    vRet            CHAR(2);
    vIDEntAdq_BM    VARCHAR(10);
    vIDEntAdq_BP    VARCHAR(10);
BEGIN
    -- VALIDA ID_ENTADQ ENVIADA POR BANCO
    IF vEntorno = 'PROD' THEN
        vIDEntAdq_BM := '1986227512';
        vIDEntAdq_BP := '1993731318';
    ELSE
        vIDEntAdq_BM := '0986227512';  -- 0986227512 pruebas naiguata IPR 1302
        vIDEntAdq_BP := '0993731318';  -- 1986231317 Sustituido 06032020 FJVG IPR1302
    END IF;
    -- DETERMINA COD_ENTADQ
    vRet := NULL;
    IF TRIM(pcodentadq_nbr) = vIDEntAdq_BM THEN
        vRet := 'BM';
    ELSIF TRIM(pcodentadq_nbr) = vIDEntAdq_BP THEN
        vRet := 'BP';
    END IF;
    RETURN vRet;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Funcion     : f_GetImporteComisionP46
-- Autor       : Jesus Munante Gonzales
-- Descripción : Retorna el Importe de Comision (+) o (-) segun Codigo de proceso
--               Además realiza el tipo de cambio respectivo.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION f_GetImporteComisionP46(
                    pimportesgn     VARCHAR2,
                    pimportenbr     VARCHAR2,
                    pentadqnbr      VARCHAR2,
                    pcodmarca       VARCHAR2,
                    pcodmoncomis    NUMBER,
                    pcodmontrans    NUMBER,
                    pfecha          DATE ) RETURN VARCHAR2
IS
    vCEntAdq        CHAR(2);
    vImporte        NUMBER;
    vTCambio        NUMBER;
    vFechaTC        DATE;
    vCMonedaComis   CHAR(3);
    vCMonedaTrans   CHAR(3);
    vRet            VARCHAR2(11);
BEGIN
    vImporte      := 0;
    vCEntAdq      := f_GetIdEntAdq(pentadqnbr);
    vFechaTC      := pfecha;
    vCMonedaComis := TO_CHAR(pcodmoncomis);
    vCMonedaTrans := TO_CHAR(pcodmontrans);
    vTCambio      := pqcomercios.gcw_f_getmonedatipocambio(vCEntAdq,pcodmarca,vCMonedaComis,vCMonedaTrans,vFechaTC);
    -- Obteniendo Importe al Tipo de Cambio
    vImporte      := TO_NUMBER(pimportenbr)*vTCambio;
    -- Creando Importe en Formato P46
    vRet := '00' || pimportesgn || LPAD(ROUND(vImporte,0),8,0);
    RETURN vRet;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Funcion     : f_GetTipoDiferencia
-- Autor       : Jesus Munante Gonzales
-- Descripción : Funcion que obtiene los tipo de diferencia por Monto o Importe.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION f_GetTipoDiferencia(preg Reg_TMP) return char is
    vTDif           CHAR(2);
    vMCPmoneda      NUMBER(3,0);
    vMCPimporte     NUMBER(12,0);
BEGIN
    IF preg.msgtype = 'FREC' THEN
        -- VENTAS
        SELECT p49montra, p04imptra
          INTO vmcpmoneda, vmcpimporte
          FROM mcp_maestro_conc mcp
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF preg.msgtype = 'EREC' THEN
        -- ANULACIONES
        SELECT p49montra, p04imptra
          INTO vmcpmoneda, vmcpimporte
          FROM mcp_maestro_conc mcp
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;

    vTDif := null;

    IF NOT (preg.p49montra = vMCPmoneda) AND NOT (preg.p04imptra = vMCPimporte OR preg.p04imptra_2 = vMCPimporte) THEN
        vTDif := 'DA';     -- CASO 1: DIFERENCIA DE MONEDA E IMPORTE
    ELSE
        IF NOT(preg.p49montra = vMCPmoneda) THEN
            vTDif := 'DM'; -- CASO 2: DIFERENCIA DE MONEDA
        ELSIF NOT(preg.p04imptra = vMCPimporte OR preg.p04imptra_2 = vMCPimporte) THEN
            vTDif := 'DI'; -- CASO 3: DIFERENCIA DE IMPORTE
        END IF;
    END IF;
    RETURN vTDif;
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : pf_Load_compensacion
--               pfecha -> Fecha de proceso
-- Autor       : Jesus Munante Gonzales
-- Descripción : Proceso para compensación, lee la tabla "TMP_MAESTRO_COMP",
--               obtiene importes y operaciones y actualiza la tabla
--               "CLR_RESUMEN_MAESTRO".
-- Precondicion: Debe de estar procesado la conciliacion de esta fecha.
-- Modificacion (2011/11 CPU - IPR 1017):
-- Ahora la carga es por archivo CTL a una tabla temporal inicial.
-- Luego se lee la tabla temporal y se carga la info a "CLR_RESUMEN_MAESTRO".
-- Actualiza los registros cargados en la temporal.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION pf_Load_compensacion(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2) RETURN VARCHAR2 IS
    TYPE Ref_CurTMP IS REF CURSOR;
    CurTMP          Ref_CurTMP;
    RegTMP          Reg_TMP_COMP;
    RegCLR          Reg_CLR;
    vfec_sesion     DATE;
    vNewLine        varchar2(300);
BEGIN

    /* PROCESO COMPENSACION MAESTRO
    *******************************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc(pcod_entadq,pfecha,'PRO','PCPSMA');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pcod_entadq||' | Fecha: '||TO_CHAR(pfecha,'DD-MM-YYYY'));

    -- VALIDACION DE PARAMETROS
    pqmonproc.inslog(vIdProc, 'M', 'Validando parametros');

    IF pcod_entadq NOT IN ('BM','BP') THEN
       verrmsg := 'Error en Parametros de Entrada';
       verrcod := '1';
       raise efinerror;
    END IF;

    -- FECHA SESION
    vfec_sesion := pfecha - 1;

    /* ELIMINACION DE DATA ANTERIOR
    *******************************/
    pqmonproc.inslog(vIdProc, 'M', 'Elimina informacion anterior en base de datos');

    DELETE FROM clr_resumen_maestro
    WHERE fec_sesion = vfec_sesion AND cod_entadq = pcod_entadq;
    COMMIT;

    /* LEYENDO INFORMACION DE TABLA TEMPORAL MAESTRO
    ************************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Leyendo informacion de tabla temporal Maestro Comp');
    OPEN CurTMP FOR
        SELECT cod_moneda,impana_s87_accc,numana_s75_accc,impcar_s88_accc,numcar_s76_accc,
            ichaab_s105_accc,nchaab_s107_accc,ichaca_s106_accc,nchaca_s108_accc,cuocar_s110_accc,
            cuoabo_s109_accc,imp_neto,tipo_saldo
        FROM tmp_maestro_comp
        WHERE fec_sesion IS NULL;

    /* INSERTANDO REGISTROS EN BD
    *****************************/
    pqmonproc.inslog(vIdProc, 'M', 'Insertando registro en base de datos');
    LOOP
    FETCH CurTMP INTO RegTMP;
    EXIT WHEN CurTMP%NOTFOUND;
        -- CALCULANDO VALORES PARA INSERTAR
        RegCLR := f_ini_RegCLR;
        -- MONEDA (Currency Code Settlement)
        RegCLR.cod_moneda           := RegTMP.cod_moneda;
        -- IMPORTE ANULACIONES (Total Debit Amount Settlement)
        RegCLR.impana_s87_accc      := RegTMP.impana_s87_accc;
        -- OPERACIONES ANULACIONES (Number of Debit Items)
        RegCLR.numana_s75_accc      := RegTMP.numana_s75_accc;
        -- IMPORTE COMPRAS (Total Credit Amount - Settlement)
        RegCLR.impcar_s88_accc      := RegTMP.impcar_s88_accc;
        -- OPERACIONES COMPRAS (Number of Credit Items)
        RegCLR.numcar_s76_accc      := RegTMP.numcar_s76_accc;
        -- Total Reversal Credit Amount
        RegCLR.ichaab_s105_accc     := RegTMP.ichaab_s105_accc;
        -- Number of Credit Reversal
        RegCLR.nchaab_s107_accc     := RegTMP.nchaab_s107_accc;
        -- Total Reversal Debit Amount
        RegCLR.ichaca_s106_accc     := RegTMP.ichaca_s106_accc;
        -- Number of Debit Reversal
        RegCLR.nchaca_s108_accc     := RegTMP.nchaca_s108_accc;
        -- COMISION CREDITO (Total Interchange Fees - Debits)
        RegCLR.cuocar_s110_accc     := RegTMP.cuocar_s110_accc;
        -- COMISION DEBITO (Total Interchange Fees - Credits)
        RegCLR.cuoabo_s109_accc     := RegTMP.cuoabo_s109_accc;
        -- IMPORTE NETO (Total Net Settlement)
        RegCLR.imp_neto             := RegTMP.imp_neto;
        -- TIPO SALDO (Total Net Settlement DR/CR Indicator)
        RegCLR.tipo_saldo           := RegTMP.tipo_saldo;
        -- Campos adicionales
        RegCLR.fec_sesion           := vfec_sesion;
        RegCLR.cod_entadq           := pcod_entadq;
        RegCLR.cod_hrcierre         := '1';
        RegCLR.tipo_lote            := 'E';
        RegCLR.cod_entidad          := '8010';
        RegCLR.num_lote             := '516';
        RegCLR.tipmov               := 'd';
        RegCLR.num_oper             := + RegCLR.numana_s75_accc
                                       + RegCLR.numcar_s76_accc
                                       + RegCLR.nchaab_s107_accc
                                       + RegCLR.nchaca_s108_accc;
        RegCLR.imp_neto_signo       := RegCLR.imp_neto * -1;
        RegCLR.imp_neto_comp        := RegCLR.imp_neto;
        RegCLR.imp_neto_signo_comp  := RegCLR.imp_neto_signo;
        RegCLR.num_oper_comp        := RegCLR.num_oper;
        RegCLR.tipo_saldo_comp      := RegCLR.tipo_saldo;

        INSERT INTO clr_resumen_maestro
                (fec_sesion,cod_entadq,cod_hrcierre,tipo_lote,cod_entidad,num_lote,
                imp_neto_signo,tipmov,tipo_saldo,cod_moneda,numcar_s76_accc,impcar_s88_accc,
                ichaab_s105_accc,ichaca_s106_accc,nchaab_s107_accc,nchaca_s108_accc,
                cuoabo_s109_accc,cuocar_s110_accc,numana_s75_accc,impana_s87_accc,
                num_oper,imp_neto,imp_neto_comp,imp_neto_signo_comp,num_oper_comp,tipo_saldo_comp)
        VALUES
                (RegCLR.fec_sesion,RegCLR.cod_entadq,RegCLR.cod_hrcierre,RegCLR.tipo_lote,
                RegCLR.cod_entidad,RegCLR.num_lote,RegCLR.imp_neto_signo,RegCLR.tipmov,
                RegCLR.tipo_saldo,RegCLR.cod_moneda,RegCLR.numcar_s76_accc,RegCLR.impcar_s88_accc,
                RegCLR.ichaab_s105_accc,RegCLR.ichaca_s106_accc,RegCLR.nchaab_s107_accc,
                RegCLR.nchaca_s108_accc,RegCLR.cuoabo_s109_accc,RegCLR.cuocar_s110_accc,
                RegCLR.numana_s75_accc,RegCLR.impana_s87_accc,RegCLR.num_oper,RegCLR.imp_neto,
                RegCLR.imp_neto_comp,RegCLR.imp_neto_signo_comp,RegCLR.num_oper_comp,
                RegCLR.tipo_saldo_comp);
        COMMIT;
    END LOOP;
    CLOSE CurTMP;

    pqmonproc.inslog(vIdProc, 'M', 'Actualizando tabla temporal Maestro Comp (fec_sesion)');

    UPDATE tmp_maestro_comp
    SET fec_sesion = vfec_sesion,
        cod_entadq = pcod_entadq,
        fec_carga = sysdate
    WHERE fec_sesion IS NULL;
    COMMIT;

    pqmonproc.inslog(vIdProc, 'M', 'Actualizacion de compensacion satisfactoria');

    -- FIN DE PROCESO
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');

    RETURN '0~';

EXCEPTION
        WHEN eFinError THEN
             pqmonproc.inslog(vIdProc, 'E', verrmsg);
             vretc := pqctlproc.updctlproc(vIdProc, 'E');
             vretc := pqmonproc.updmonproc(vIdProc, 'E', vErrCod);
             RETURN 'EERROR: '||verrmsg||'~';

        WHEN others THEN
             voracode := abs(sqlcode);
             voraerr  := substr(sqlerrm,1,200);
             pqmonproc.inslog(vIdProc, 'E', voraerr);
             vretc    := pqctlproc.updctlproc(vIdProc, 'E');
             vretc    := pqmonproc.updmonproc(vIdProc, 'E', '99');
             RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : pf_Load_conciliacion
-- Autor       : Jesus Munante Gonzales
-- Descripción : Proceso para conciliación entre las transacciones de "MAESTRO"
--               y la "PROCESADORA". Se consideran los siguientes valores para
--               los tipos de diferencias:
--
--                  EM = Existe en "MAESTRO" y no en "PROCESADORA".
--                  DM = Diferencia por Moneda.
--                  DI = Diferencia por Importe.
--                  DA = Diferencia por Moneda e Importe.
--
--               Se considera que en la "PROCESADORA" ningun registro se repite.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FUNCTION pf_Load_conciliacion(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2:=NULL) RETURN VARCHAR2 IS
    TYPE Ref_CurTMP IS REF CURSOR;
    CurTMP          Ref_CurTMP;
    RegTMP          Reg_TMP;
    vTipDif         CHAR(2);
    vNCont          NUMBER  := 0;
    vTotReg         NUMBER  := 0;
    vTotRegVTA      NUMBER  := 0;
    vTotRegANU      NUMBER  := 0;
    vTotRegCCL      NUMBER  := 0;
    vTotRegDIF      NUMBER  := 0;
BEGIN

    /* PROCESO CONCILIACION MAESTRO
    *******************************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc(pcod_entadq,pfecha,'PRO','PCCLMA');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pcod_entadq||' | Fecha: '||TO_CHAR(pfecha,'DD-MM-YYYY'));
    -- VALIDACION DE PARAMETROS
    pqmonproc.inslog(vIdProc, 'M', 'Validando parametros');
    IF pcod_entadq NOT IN ('BM','BP') THEN
       verrmsg := 'Error en Parametros de Entrada';
       verrcod := '1';
       raise efinerror;
    END IF;

    /* PREPARA DATA DE MOVIMIENTOS
    ******************************/

    -- COPIA MOVIMIENTOS DE MCP
    pqmonproc.inslog(vIdProc, 'M', 'Copia movimientos de MCP');
    p_CopyMovimientosMCP(pfecha, pcod_entadq);
    -- REESTABLECE COMO PENDIENTES DATOS CONCILIADOS ANTES
    pqmonproc.inslog(vIdProc, 'M', 'Reestablece como Pendientes datos anteriores');
    UPDATE mcp_maestro_conc
       SET flg_concil = 'P',
           fec_concil = NULL
     WHERE cod_entadq = pcod_entadq
       AND fec_concil = pfecha;
    COMMIT;
    -- LIMPIA TABLA MAESTRO DIFERENCIAS
    pqmonproc.inslog(vIdProc, 'M', 'Elimina Datos anteriores en tabla de diferencias');
    DELETE FROM mcp_maestro_diff
     WHERE cod_entadq = pcod_entadq
       AND fec_concil = pfecha;
    COMMIT;

    /* PREPARA TABLA TEMPORAL MAESTRO
    *********************************/
    pqmonproc.inslog(vIdProc, 'M', 'Prepara tabla temporal de Maestro');
    UPDATE tmp_maestro
       SET p28sesion  = TO_CHAR (TO_DATE (p12timloc_date, 'MMDDYY'), 'YYYYMMDD'),
           p12timloc  = TO_CHAR (TO_DATE (p12timloc_date, 'MMDDYY'), 'YYYYMMDD')||p12timloc_time,
           p32idadq   = f_getidentadq (p32idadq_nbr),
           p46tcuot01 = f_getimportecomisionp46 (p46tcuot01_sgn, p46tcuot01_nbr, p32idadq_nbr, 'MC', p50moncon, p49montra, TO_DATE (p12timloc_date, 'MMDDYY'));
    COMMIT;

    /* LEYENDO INFORMACION DE TABLA TEMPORAL MAESTRO
    ************************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Leyendo informacion de tabla temporal Maestro');
    OPEN CurTMP FOR
        SELECT nbr, msgtype, p02numtar, p03codpro, p04imptra, p04imptra_2, p05impcon,
            p09concon, p11idetra, p11idetra_2, p12timloc, p12timloc_date,
            p12timloc_time, p18lacti, p22punser, p28sesion, p32idadq, p32idadq_nbr,
            p37datref, p38numaut, p39codacr, p41cseri, p46tcuot01, p46tcuot01_nbr,
            p46tcuot01_sgn, p49montra, p50moncon
        FROM tmp_maestro;

    /* REALIZANDO CRUCE MAESTRO Y PROCESADORA
    *****************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Realizando cruce entre Maestro y Procesadora');
    LOOP
    FETCH CurTMP INTO RegTMP;
    EXIT WHEN CurTMP%NOTFOUND;
        -- CANTIDAD DE REGISTROS
        vTotReg := vTotReg + 1;
        IF RegTMP.MSGTYPE = 'FREC' THEN
           vTotRegVTA := vTotRegVTA + 1;
        ELSIF RegTMP.MSGTYPE = 'EREC' THEN
              vTotRegANU := vTotRegANU + 1;
        END IF;
        -- BUSCAMOS EN "PROCESADORA"
        vNCont   := f_GetBuscaNumRegistros(RegTMP);
        IF vNCont = 0 THEN
            -- EXISTE EN "MAESTRO" PERO NO EN "PROCESADORA"
            vTotRegDIF := vTotRegDIF + 1;
            p_SaveDiferencia(RegTMP,pfecha,'EM');
        ELSIF vNCont=1 THEN
            -- EXISTE EN "MAESTRO" Y "PROCESADORA"
            vTipDif:= f_GetTipoDiferencia(RegTMP);
            IF vTipDif is null THEN
                -- REGISTRA CONCILIACION OK
                vTotRegCCL := vTotRegCCL + 1;
                p_SaveConciliacion(RegTMP,pfecha);
            ELSE
                -- REGISTRA DIFERENCIA
                vTotRegDIF := vTotRegDIF + 1;
                p_SaveDiferencia(RegTMP,pfecha,vTipDif);
            END IF;
        END IF;
    END LOOP;
    CLOSE CurTMP;

    -- RESUMEN
    pqmonproc.inslog(vIdProc, 'M', 'Resumen de Carga:');
    pqmonproc.inslog(vIdProc, 'M', ' - Total : '  ||LTRIM(TO_CHAR(vTotReg,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Ventas: '     ||LTRIM(TO_CHAR(vTotRegVTA,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Anulaciones: '||LTRIM(TO_CHAR(vTotRegANU,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', 'Resumen de Conciliacion:');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Conciliadas: '||LTRIM(TO_CHAR(vTotRegCCL,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Diferencias: '||LTRIM(TO_CHAR(vTotRegDIF,'999,999'))||' registros');

    pqmonproc.inslog(vIdProc, 'M', 'Conciliacion satisfactoria');

    -- FIN DE PROCESO
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');

    RETURN '0~';

EXCEPTION

        WHEN eFinError THEN
             pqmonproc.inslog(vIdProc, 'E', verrmsg);
             vretc := pqctlproc.updctlproc(vIdProc, 'E');
             vretc := pqmonproc.updmonproc(vIdProc, 'E', vErrCod);
             RETURN 'EERROR: '||verrmsg||'~';

        WHEN others THEN
             voracode := abs(sqlcode);
             voraerr  := substr(sqlerrm,1,200);
             pqmonproc.inslog(vIdProc, 'E', voraerr);
             vretc    := pqctlproc.updctlproc(vIdProc, 'E');
             vretc    := pqmonproc.updmonproc(vIdProc, 'E', '99');
             RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Procedure   : p_NoExisteArchivo
-- Autor       : Jesus Munante Gonzales
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

PROCEDURE p_NoExisteArchivo(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2, pcod_proc VARCHAR2) as
BEGIN
    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc(pcod_entadq,pfecha,'PRO',pcod_proc);
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pcod_entadq||' | Fecha: '||TO_CHAR(pfecha,'DD-MM-YYYY'));

    -- REGISTRO DE ERROR
    verrmsg := 'No se encuentra el archivo: '||pfile;
    verrcod := '4';
    pqmonproc.inslog(vIdProc, 'E', vErrMsg);
    vretc := pqctlproc.updctlproc(vIdProc, 'E');
    vretc := pqmonproc.updmonproc(vIdProc, 'E', vErrCod);
END;

--INICIO NAIGUATA
FUNCTION pf_Load_conciliacion_ngta(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2:=NULL) RETURN VARCHAR2 IS
    TYPE Ref_CurTMP IS REF CURSOR;
    CurTMP          Ref_CurTMP;
    RegTMP          Reg_TMP;
    vTipDif         CHAR(2);
    vNCont          NUMBER  := 0;
    vTotReg         NUMBER  := 0;
    vTotRegVTA      NUMBER  := 0;
    vTotRegANU      NUMBER  := 0;
    vTotRegCCL      NUMBER  := 0;
    vTotRegDIF      NUMBER  := 0;
BEGIN

    /* PROCESO CONCILIACION MAESTRO NAIGUATA
    *****************************************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc(pcod_entadq,pfecha,'PRO','PCCLMA_NGTA');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pcod_entadq||' | Fecha: '||TO_CHAR(pfecha,'DD-MM-YYYY'));
    -- VALIDACION DE PARAMETROS
    pqmonproc.inslog(vIdProc, 'M', 'Validando parametros Conc. Naiguata');
    IF pcod_entadq NOT IN ('BM','BP') THEN
       verrmsg := 'Error en Parametros de Entrada Conc. Naiguata';
       verrcod := '1';
       raise efinerror;
    END IF;

    /* PREPARA DATA DE MOVIMIENTOS
    ******************************/

    -- COPIA MOVIMIENTOS DE MCP
    pqmonproc.inslog(vIdProc, 'M', 'Copia movimientos de MCP Naiguata');
    p_CopyMovimientosMCP_Ngta(pfecha, pcod_entadq);
    -- REESTABLECE COMO PENDIENTES DATOS CONCILIADOS ANTES
    pqmonproc.inslog(vIdProc, 'M', 'Reestablece como Pendientes datos anteriores de Naiguata');
    UPDATE mcp_maestro_conc_ngta
       SET flg_concil = 'P',
           fec_concil = NULL
     WHERE cod_entadq = pcod_entadq
       AND fec_concil = pfecha;
    COMMIT;
    -- LIMPIA TABLA MAESTRO DIFERENCIAS NAIGUATA
    pqmonproc.inslog(vIdProc, 'M', 'Elimina Datos anteriores en tabla de diferencias de Naiguata');
    DELETE FROM mcp_maestro_diff_ngta
     WHERE cod_entadq = pcod_entadq
       AND fec_concil = pfecha;
    COMMIT;

    /* PREPARA TABLA TEMPORAL MAESTRO NAIGUATA
    ******************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Prepara tabla temporal de Maestro Naiguata');
    UPDATE tmp_maestro_ngta
       SET p28sesion  = TO_CHAR (TO_DATE (p12timloc_date, 'MMDDYY'), 'YYYYMMDD'),
           p12timloc  = TO_CHAR (TO_DATE (p12timloc_date, 'MMDDYY'), 'YYYYMMDD')||p12timloc_time,
           p32idadq   = f_getidentadq (p32idadq_nbr),
           p46tcuot01 = f_getimportecomisionp46 (p46tcuot01_sgn, p46tcuot01_nbr, p32idadq_nbr, 'MC', p50moncon, p49montra, TO_DATE (p12timloc_date, 'MMDDYY'));
    COMMIT;

    /* LEYENDO INFORMACION DE TABLA TEMPORAL MAESTRO NAIGUATA
    *********************************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Leyendo informacion de tabla temporal Maestro Naiguata');
    OPEN CurTMP FOR
        -- GLOBALR MODIFICADO 04-12-2014 IPR1061 Se inlcuye el p60codraz al cursor
        SELECT nbr, msgtype, p02numtar, p03codpro, p04imptra, p04imptra_2, p05impcon,
            p09concon, p11idetra, p11idetra_2, p12timloc, p12timloc_date,
            p12timloc_time, p18lacti, p22punser, p28sesion, p32idadq, p32idadq_nbr,
            p37datref, p38numaut, p39codacr, p41cseri, p46tcuot01, p46tcuot01_nbr,
            p46tcuot01_sgn, p49montra, p50moncon  --, p60codraz
        FROM tmp_maestro_ngta;

    /* REALIZANDO CRUCE MAESTRO NAIGUATA Y PROCESADORA
    **************************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Realizando cruce entre Maestro Naiguata y Procesadora');
    LOOP
    FETCH CurTMP INTO RegTMP;
    EXIT WHEN CurTMP%NOTFOUND;
        -- CANTIDAD DE REGISTROS
        vTotReg := vTotReg + 1;
        IF RegTMP.MSGTYPE = 'FREC' THEN
           vTotRegVTA := vTotRegVTA + 1;
        ELSIF RegTMP.MSGTYPE = 'EREC' THEN
              vTotRegANU := vTotRegANU + 1;
        END IF;
        -- BUSCAMOS EN "PROCESADORA"
        vNCont   := f_GetBuscaNumRegistros_Ngta(RegTMP);
        IF vNCont = 0 THEN
            -- EXISTE EN "MAESTRO NAIGUATA" PERO NO EN "PROCESADORA"
            vTotRegDIF := vTotRegDIF + 1;
            p_SaveDiferencia_Ngta(RegTMP,pfecha,'EM');
        ELSIF vNCont=1 THEN
            -- EXISTE EN "MAESTRO NAIGUATA" Y "PROCESADORA"
            vTipDif:= f_GetTipoDiferencia_Ngta(RegTMP);
            IF vTipDif is null THEN
                -- REGISTRA CONCILIACION OK
                vTotRegCCL := vTotRegCCL + 1;
                p_SaveConciliacion_Ngta(RegTMP,pfecha);
            ELSE
                -- REGISTRA DIFERENCIA
                vTotRegDIF := vTotRegDIF + 1;
                p_SaveDiferencia_Ngta(RegTMP,pfecha,vTipDif);
            END IF;
        END IF;
    END LOOP;
    CLOSE CurTMP;

    -- RESUMEN
    pqmonproc.inslog(vIdProc, 'M', 'Resumen de Carga Naiguata:');
    pqmonproc.inslog(vIdProc, 'M', ' - Total : '  ||LTRIM(TO_CHAR(vTotReg,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Ventas Naiguata: '     ||LTRIM(TO_CHAR(vTotRegVTA,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Anulaciones Naiguata: '||LTRIM(TO_CHAR(vTotRegANU,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', 'Resumen de Conciliacion Naiguata:');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Conciliadas Naiguata: '||LTRIM(TO_CHAR(vTotRegCCL,'999,999'))||' registros');
    pqmonproc.inslog(vIdProc, 'M', ' - Total Diferencias Naiguata: '||LTRIM(TO_CHAR(vTotRegDIF,'999,999'))||' registros');

    pqmonproc.inslog(vIdProc, 'M', 'Conciliacion Naiguata satisfactoria');

    -- FIN DE PROCESO
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');

    RETURN '0~';

EXCEPTION

        WHEN eFinError THEN
             pqmonproc.inslog(vIdProc, 'E', verrmsg);
             vretc := pqctlproc.updctlproc(vIdProc, 'E');
             vretc := pqmonproc.updmonproc(vIdProc, 'E', vErrCod);
             RETURN 'EERROR: '||verrmsg||'~';

        WHEN others THEN
             voracode := abs(sqlcode);
             voraerr  := substr(sqlerrm,1,200);
             pqmonproc.inslog(vIdProc, 'E', voraerr);
             vretc    := pqctlproc.updctlproc(vIdProc, 'E');
             vretc    := pqmonproc.updmonproc(vIdProc, 'E', '99');
             RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;

PROCEDURE p_CopyMovimientosMCP_Ngta(pfecha DATE, pbanco VARCHAR2) AS
    vcant       PLS_INTEGER := 0;
    vest_proc   CHAR        := NULL;
    vfecha      DATE;
BEGIN
    -- ACTUALIZA TABLA MAESTRO CONCILIACION
    FOR ndias IN REVERSE 0..5 LOOP
        vfecha := pfecha - ndias;
        -- VERIFICA DIA PROCESADO
        BEGIN
           SELECT est_proc
             INTO vest_proc
             FROM ctl_procesos
            WHERE cod_entadq = pbanco
              AND fecha = vfecha
              AND cod_proc = 'PCCLMA_NGTA';
        EXCEPTION
           WHEN OTHERS THEN
                vest_proc := 'X';
        END;
        -- VERIFICA ESTADO FINALIZADO
        IF NVL(vest_proc,'X') <> 'F' THEN
           SELECT COUNT (*)
             INTO vcant
             FROM mcp_maestro_conc_ngta
            WHERE cod_entadq = pbanco AND p28sesion = TO_CHAR(vfecha-1,'YYYYMMDD');
        END IF;
        -- COPY DATA
        IF NVL(vest_proc,'X') <> 'F' AND vcant = 0 THEN
            -- BANCO MERCANTIL
            IF pbanco='BM' THEN
                INSERT INTO mcp_maestro_conc_ngta
                (id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, flg_concil, fec_sesion)
                SELECT
                id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, 'P', TO_DATE(p28sesion,'YYYYMMDD')
                FROM mcp_bm
                WHERE cod_entadq = 'BM'
                AND p28sesion = TO_CHAR(vfecha-1,'YYYYMMDD')
                AND p48tiptra LIKE '10%'
                AND p71nummen LIKE '9010%'; --Ajuste IPR1302 06032020 FJVG
            -- BANCO PROVINCIAL
            ELSIF pbanco='BP' THEN
                INSERT INTO mcp_maestro_conc_ngta  --Ajuste IPR1302 06032020 FJVG
                (id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, flg_concil, fec_sesion)
                SELECT
                id_mov, cod_entadq, csb_entadq, cod_hrcierre, p00idmsg,
                p02numtar, p03codpro, p04imptra, p05impcon, p06imptit, p09concon,
                p10concli, p11idetra, p12timloc, p14feccad, p16feccon, p17feccap,
                p18lacti, p22punser, p24codfun, p25codraz, p26lacti, p28sesion,
                p29inlote, p30impori, p31refadq, p32idadq, p33idpre, p37datref,
                p38numaut, p39codacr, p41cseri, p42ideest, p46tcuot01,
                p46tcuot02, p46tcuot03, p46tcuot04, p48tiptra, p48cuenta,
                p48tipmov, p48filler, p49montra, p50moncon, p51montit,
                p56do_idmsg, p56do_idetra, p56do_timloc, p56do_idadq, p58idaut,
                p62melect, p71nummen, pxxfiller, tipo_insert, es_dataok,
                tipo_auxfunc, 'P', TO_DATE(p28sesion,'YYYYMMDD')
                FROM mcp_bp
                WHERE cod_entadq = 'BP' 
                AND p28sesion = TO_CHAR(vfecha-1,'YYYYMMDD')
                AND p48tiptra LIKE '10%'
                AND p71nummen LIKE '9010%'; --Ajuste IPR1302 06032020 fjvg
            END IF;
            COMMIT;
        END IF;
    END LOOP;

END;

PROCEDURE p_SaveDiferencia_Ngta( preg Reg_TMP, pfecha DATE, ptipodiff VARCHAR2) AS
BEGIN
    INSERT INTO mcp_maestro_diff_ngta
            (p28sesion, cod_entadq, p02numtar, p03codpro,
             p04imptra, p05impcon, p09concon, p11idetra,
             p12timloc, p18lacti, p22punser, p32idadq,
             p37datref, p38numaut, p39codacr, p41cseri,
             p46tcuot01, p49montra, p50moncon, tipo_diff,
             fec_sesion, fec_concil, msgtype, p04imptra_2
            )
     VALUES (preg.p28sesion, preg.p32idadq, preg.p02numtar, preg.p03codpro,
             preg.p04imptra, preg.p05impcon, preg.p09concon, preg.p11idetra,
             preg.p12timloc, preg.p18lacti, preg.p22punser, preg.p32idadq,
             preg.p37datref, preg.p38numaut, preg.p39codacr, preg.p41cseri,
             preg.p46tcuot01, preg.p49montra, preg.p50moncon, ptipodiff,
             TO_DATE (preg.p28sesion, 'YYYYMMDD'), pfecha, preg.msgtype, preg.p04imptra_2
            );
COMMIT ;

END;

FUNCTION f_GetBuscaNumRegistros_Ngta(preg Reg_TMP) return number is
    vncont  NUMBER(7);
BEGIN
    IF (preg.msgtype = 'FREC') THEN
        -- VENTAS
        SELECT NVL (COUNT (*), 0)
          INTO vncont
          FROM mcp_maestro_conc_ngta mcp
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF (preg.msgtype = 'EREC') THEN
        -- ANULACIONES
        SELECT NVL (COUNT (*), 0)
          INTO vncont
          FROM mcp_maestro_conc_ngta mcp
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;
    RETURN vncont;
END;

FUNCTION f_GetTipoDiferencia_Ngta(preg Reg_TMP) return char is
    vTDif           CHAR(2);
    vMCPmoneda      NUMBER(3,0);
    vMCPimporte     NUMBER(12,0);
BEGIN
    IF preg.msgtype = 'FREC' THEN
        -- VENTAS
        SELECT p49montra, p04imptra
          INTO vmcpmoneda, vmcpimporte
          FROM mcp_maestro_conc_ngta mcp
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF preg.msgtype = 'EREC' THEN
        -- ANULACIONES
        SELECT p49montra, p04imptra
          INTO vmcpmoneda, vmcpimporte
          FROM mcp_maestro_conc_ngta mcp
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;

    vTDif := null;

    IF NOT (preg.p49montra = vMCPmoneda) AND NOT (preg.p04imptra = vMCPimporte OR preg.p04imptra_2 = vMCPimporte) THEN
        vTDif := 'DA';     -- CASO 1: DIFERENCIA DE MONEDA E IMPORTE
    ELSE
        IF NOT(preg.p49montra = vMCPmoneda) THEN
            vTDif := 'DM'; -- CASO 2: DIFERENCIA DE MONEDA
        ELSIF NOT(preg.p04imptra = vMCPimporte OR preg.p04imptra_2 = vMCPimporte) THEN
            vTDif := 'DI'; -- CASO 3: DIFERENCIA DE IMPORTE
        END IF;
    END IF;
    RETURN vTDif;
END;

FUNCTION pf_Load_compensacion_ngta(pcod_entadq VARCHAR2, pfecha DATE, pfile VARCHAR2) RETURN VARCHAR2 IS
    TYPE Ref_CurTMP IS REF CURSOR;
    CurTMP          Ref_CurTMP;
    RegTMP          Reg_TMP_COMP;
    RegCLR          Reg_CLR;
    vfec_sesion     DATE;
    vNewLine        varchar2(300);
BEGIN

    /* PROCESO COMPENSACION MAESTRO NAIGUATA
    ****************************************/

    -- OBTIENE ID DE PROCESO Y LO REGISTRA EN TABLA DE CONTROL
    vIdProc := pqctlproc.getidproc(pcod_entadq,pfecha,'PRO','PCPSMA_NGTA');
    pqmonproc.inslog(vIdProc, 'M', 'EntAdq: '||pcod_entadq||' | Fecha: '||TO_CHAR(pfecha,'DD-MM-YYYY'));

    -- VALIDACION DE PARAMETROS
    pqmonproc.inslog(vIdProc, 'M', 'Validando parametros de Naiguata');

    IF pcod_entadq NOT IN ('BM','BP') THEN
       verrmsg := 'Error en Parametros de Entrada de Naiguata';
       verrcod := '1';
       raise efinerror;
    END IF;

    -- FECHA SESION
    vfec_sesion := pfecha - 1;

    /* ELIMINACION DE DATA ANTERIOR
    *******************************/
    pqmonproc.inslog(vIdProc, 'M', 'Elimina informacion anterior de Naiguata en base de datos');

    DELETE FROM clr_resumen_maestro_ngta
    WHERE fec_sesion = vfec_sesion AND cod_entadq = pcod_entadq;
    COMMIT;

    /* LEYENDO INFORMACION DE TABLA TEMPORAL MAESTRO
    ************************************************/
    pqmonproc.inslog(vIdProc, 'M', 'Leyendo informacion de tabla temporal Maestro Comp Ngta');
    OPEN CurTMP FOR
        SELECT cod_moneda,impana_s87_accc,numana_s75_accc,impcar_s88_accc,numcar_s76_accc,
            ichaab_s105_accc,nchaab_s107_accc,ichaca_s106_accc,nchaca_s108_accc,cuocar_s110_accc,
            cuoabo_s109_accc,imp_neto,tipo_saldo
        FROM tmp_maestro_comp_ngta
        WHERE fec_sesion IS NULL;

    /* INSERTANDO REGISTROS EN BD
    *****************************/
    pqmonproc.inslog(vIdProc, 'M', 'Insertando registro de Naiguata en base de datos');
    LOOP
    FETCH CurTMP INTO RegTMP;
    EXIT WHEN CurTMP%NOTFOUND;
        -- CALCULANDO VALORES PARA INSERTAR
        RegCLR := f_ini_RegCLR;
        -- MONEDA (Currency Code Settlement)
        RegCLR.cod_moneda           := RegTMP.cod_moneda;
        -- IMPORTE ANULACIONES (Total Debit Amount Settlement)
        RegCLR.impana_s87_accc      := RegTMP.impana_s87_accc;
        -- OPERACIONES ANULACIONES (Number of Debit Items)
        RegCLR.numana_s75_accc      := RegTMP.numana_s75_accc;
        -- IMPORTE COMPRAS (Total Credit Amount - Settlement)
        RegCLR.impcar_s88_accc      := RegTMP.impcar_s88_accc;
        -- OPERACIONES COMPRAS (Number of Credit Items)
        RegCLR.numcar_s76_accc      := RegTMP.numcar_s76_accc;
        -- Total Reversal Credit Amount
        RegCLR.ichaab_s105_accc     := RegTMP.ichaab_s105_accc;
        -- Number of Credit Reversal
        RegCLR.nchaab_s107_accc     := RegTMP.nchaab_s107_accc;
        -- Total Reversal Debit Amount
        RegCLR.ichaca_s106_accc     := RegTMP.ichaca_s106_accc;
        -- Number of Debit Reversal
        RegCLR.nchaca_s108_accc     := RegTMP.nchaca_s108_accc;
        -- COMISION CREDITO (Total Interchange Fees - Debits)
        RegCLR.cuocar_s110_accc     := RegTMP.cuocar_s110_accc;
        -- COMISION DEBITO (Total Interchange Fees - Credits)
        RegCLR.cuoabo_s109_accc     := RegTMP.cuoabo_s109_accc;
        -- IMPORTE NETO (Total Net Settlement)
        RegCLR.imp_neto             := RegTMP.imp_neto;
        -- TIPO SALDO (Total Net Settlement DR/CR Indicator)
        RegCLR.tipo_saldo           := RegTMP.tipo_saldo;
        -- Campos adicionales
        RegCLR.fec_sesion           := vfec_sesion;
        RegCLR.cod_entadq           := pcod_entadq;
        RegCLR.cod_hrcierre         := '1';
        RegCLR.tipo_lote            := 'E';
        RegCLR.cod_entidad          := '9010'; --AJUSTES IPR1302 FJVG 06032020
        RegCLR.num_lote             := '512';  --AJUSTES IPR1302 FJVG 06032020
        RegCLR.tipmov               := 'd';
        RegCLR.num_oper             := + RegCLR.numana_s75_accc
                                       + RegCLR.numcar_s76_accc
                                       + RegCLR.nchaab_s107_accc
                                       + RegCLR.nchaca_s108_accc;
        RegCLR.imp_neto_signo       := RegCLR.imp_neto * -1;
        RegCLR.imp_neto_comp        := RegCLR.imp_neto;
        RegCLR.imp_neto_signo_comp  := RegCLR.imp_neto_signo;
        RegCLR.num_oper_comp        := RegCLR.num_oper;
        RegCLR.tipo_saldo_comp      := RegCLR.tipo_saldo;

        INSERT INTO clr_resumen_maestro_ngta
                (fec_sesion,cod_entadq,cod_hrcierre,tipo_lote,cod_entidad,num_lote,
                imp_neto_signo,tipmov,tipo_saldo,cod_moneda,numcar_s76_accc,impcar_s88_accc,
                ichaab_s105_accc,ichaca_s106_accc,nchaab_s107_accc,nchaca_s108_accc,
                cuoabo_s109_accc,cuocar_s110_accc,numana_s75_accc,impana_s87_accc,
                num_oper,imp_neto,imp_neto_comp,imp_neto_signo_comp,num_oper_comp,tipo_saldo_comp)
        VALUES
                (RegCLR.fec_sesion,RegCLR.cod_entadq,RegCLR.cod_hrcierre,RegCLR.tipo_lote,
                RegCLR.cod_entidad,RegCLR.num_lote,RegCLR.imp_neto_signo,RegCLR.tipmov,
                RegCLR.tipo_saldo,RegCLR.cod_moneda,RegCLR.numcar_s76_accc,RegCLR.impcar_s88_accc,
                RegCLR.ichaab_s105_accc,RegCLR.ichaca_s106_accc,RegCLR.nchaab_s107_accc,
                RegCLR.nchaca_s108_accc,RegCLR.cuoabo_s109_accc,RegCLR.cuocar_s110_accc,
                RegCLR.numana_s75_accc,RegCLR.impana_s87_accc,RegCLR.num_oper,RegCLR.imp_neto,
                RegCLR.imp_neto_comp,RegCLR.imp_neto_signo_comp,RegCLR.num_oper_comp,
                RegCLR.tipo_saldo_comp);
        COMMIT;
    END LOOP;
    CLOSE CurTMP;

    pqmonproc.inslog(vIdProc, 'M', 'Actualizando tabla temporal Maestro Comp Ngta (fec_sesion)');

    UPDATE tmp_maestro_comp_ngta
    SET fec_sesion = vfec_sesion,
        cod_entadq = pcod_entadq,
        fec_carga = sysdate
    WHERE fec_sesion IS NULL;
    COMMIT;

    pqmonproc.inslog(vIdProc, 'M', 'Actualizacion de compensacion naiguata satisfactoria');

    -- FIN DE PROCESO
    vretc  := pqctlproc.updctlproc(vIdProc,'F');
    vretc  := pqmonproc.updmonproc(vIdProc,'F');

    RETURN '0~';

EXCEPTION
        WHEN eFinError THEN
             pqmonproc.inslog(vIdProc, 'E', verrmsg);
             vretc := pqctlproc.updctlproc(vIdProc, 'E');
             vretc := pqmonproc.updmonproc(vIdProc, 'E', vErrCod);
             RETURN 'EERROR: '||verrmsg||'~';

        WHEN others THEN
             voracode := abs(sqlcode);
             voraerr  := substr(sqlerrm,1,200);
             pqmonproc.inslog(vIdProc, 'E', voraerr);
             vretc    := pqctlproc.updctlproc(vIdProc, 'E');
             vretc    := pqmonproc.updmonproc(vIdProc, 'E', '99');
             RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;

PROCEDURE p_SaveConciliacion_Ngta(preg Reg_TMP, pfecha DATE) AS
BEGIN
    -- ACTUALIZA ESTADO EN TABLA MAESTRO CONCILIACION NAIGUATA

    IF (preg.msgtype = 'FREC') THEN
        -- VENTA
        UPDATE mcp_maestro_conc_ngta mcp
           SET flg_concil = 'C',
               fec_concil = pfecha,
               p03codpro_maestro = preg.p03codpro,
               p04imptra_maestro = preg.p04imptra,
               p46tcuot01_maestro = preg.p46tcuot01
         WHERE mcp.p12timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p11idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    ELSIF (preg.msgtype = 'EREC') THEN
        -- ANULACION
        UPDATE mcp_maestro_conc_ngta mcp
           SET flg_concil = 'C',
               fec_concil = pfecha,
               p03codpro_maestro = preg.p03codpro,
               p04imptra_maestro = preg.p04imptra,
               p46tcuot01_maestro = preg.p46tcuot01
         WHERE mcp.p56do_timloc = preg.p12timloc
           AND mcp.p02numtar = preg.p02numtar
           AND mcp.p56do_idetra = preg.p11idetra
           AND mcp.cod_entadq = preg.p32idadq;
    END IF;
    COMMIT;
END;
--FIN    NAIGUATA

END;
/
GRANT EXECUTE ON SGCVNZ.PQPMAESTRO TO ROLE_SOPTECN;
