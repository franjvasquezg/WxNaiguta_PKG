CREATE OR REPLACE PACKAGE SGCVNZ.pqpclr_hora is

 type vP04OUT_ARRAY is varray(3) of number;
 type vP46OUT_ARRAY is varray(3) of number;
 type vNUMREG_ARRAY is varray(3) of number;
 type vSIGNO_ARRAY  is varray(3) of char(1);


  procedure f_findadq(ptabla varchar2, vbanco varchar2, vcad1 out varchar2,vcad2 out varchar2) ;

  PROCEDURE pp_clr_merchant_hora (pfecha DATE, pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq CHAR);
  PROCEDURE pp_clr_emisor_hora (pfecha DATE,pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq char);
  PROCEDURE get_anulacompra_e( tabla VARCHAR2,  phra_proceso CHAR, videmen_p00_actc NUMBER,
                             vcodpro_p03_actc VARCHAR2, vcodpro_p03_actc_01 VARCHAR2,
                             vlote VARCHAR2,  vmoneda VARCHAR2,  vemisor VARCHAR2,           vfecha VARCHAR2,
                             pcod_hrcierre VARCHAR2, padqui varchar2,
                             pP04OUT OUT vP04OUT_ARRAY,
                             pP46OUT OUT vP46OUT_ARRAY,
                             pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_anulacompra_m( tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2,   vcodpro_p03_actc_01 VARCHAR2,
                             vlote VARCHAR2, vmoneda VARCHAR2,        vfecha VARCHAR2,             pcod_hrcierre VARCHAR2,
                             padqui varchar2,
                             pP04OUT OUT vP04OUT_ARRAY,
                             pP46OUT OUT vP46OUT_ARRAY,
                             pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_cobro_e( tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                       vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                       vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                       pP46OUT OUT vP46OUT_ARRAY,
                       pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_compra_e(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2,  vcodpro_p03_actc_01 VARCHAR2,
                        vlote VARCHAR2, vmoneda VARCHAR2,        vemisor VARCHAR2,           vfecha VARCHAR2,
                        pcod_hrcierre VARCHAR2,  padqui varchar2,
                        pP04OUT OUT vP04OUT_ARRAY,
                        pP46OUT OUT vP46OUT_ARRAY,
                        pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_compra_m(tabla varchar2, phra_proceso CHAR, videmen_p00_actc number,   vcodpro_p03_actc varchar2,
                       vcodpro_p03_actc_01 varchar2,  vlote  varchar2,           vmoneda  varchar2,
                       vfecha varchar2,               pcod_hrcierre varchar2,       padqui  varchar2,
                       pP04OUT OUT vP04OUT_ARRAY,
                       pP46OUT OUT vP46OUT_ARRAY,
                       pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_copa_m(tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                     vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                     vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                     pP46OUT OUT vP46OUT_ARRAY,
                     pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_devolucion_e(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER,   vcodpro_p03_actc VARCHAR2,  vcodpro_p03_actc_01 VARCHAR2,
                            vlote VARCHAR2,      vmoneda VARCHAR2,          vemisor VARCHAR2,           vfecha VARCHAR2,
                            pcod_hrcierre VARCHAR2, padqui varchar2,
                            pP04OUT OUT vP04OUT_ARRAY,
                            pP46OUT OUT vP46OUT_ARRAY,
                            pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_devolucion_m(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2 ,   vcodpro_p03_actc_01 VARCHAR2,
                           vlote VARCHAR2,         vmoneda VARCHAR2,        vfecha VARCHAR2,              pcod_hrcierre VARCHAR2,
                           padqui varchar2,
                           pP04OUT OUT vP04OUT_ARRAY,
                           pP46OUT OUT vP46OUT_ARRAY,
                           pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE get_pago_e(tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                      vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                      vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                      pP46OUT OUT vP46OUT_ARRAY,
                      pNUMREG OUT vNUMREG_ARRAY);
  PROCEDURE pp_insert_clrletota_hora(
                              vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char);
  PROCEDURE pp_insert_clrletota_hora_misc (
                              vcod_hrcierre CHAR,           phra_proceso CHAR,           videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char);
  PROCEDURE pp_insert_clrlmtota_hora(
                              vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char);
  PROCEDURE pp_insert_clrlmtota_hora_misc (
                                    vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                                    vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                                    vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                                    vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                                    vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                                    vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                                    vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                                    vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                                    vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                                    vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                                    vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                                    vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                                    vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                                    vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                                    pcod_entadq char);
  PROCEDURE pp_main (pfecha DATE, pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq CHAR);
  FUNCTION f_clr_gendatrpt (pEntAdq CHAR, pCodHrCierre CHAR, phra_proceso CHAR, pFecSesion DATE) RETURN CHAR;
  FUNCTION f_clr_rptcuadre (pEntAdq CHAR, pCodHrCierre CHAR, phra_proceso CHAR, pFecSesion DATE) RETURN CHAR;
 function f_interfaz_pliq (pFecSesion DATE, pCodHRCierre CHAR,phra_proceso CHAR, pOpcRepro CHAR:=NULL) return char;
  FUNCTION f_findprov(pINDPRE_P48_ACTC VARCHAR2, pFILLER_P48_ACTC VARCHAR2, pNUMGUI_P48_ACTC VARCHAR2, pP4899FILLER VARCHAR2) RETURN NUMBER;
  FUNCTION f_findcash(pMCASHB_P48_ACTC VARCHAR2, pP4899FILLER VARCHAR2) RETURN NUMBER;
 end;
/


GRANT EXECUTE ON SGCVNZ.PQPCLR_HORA TO ROLE_SOPTECN;

CREATE OR REPLACE PACKAGE BODY SGCVNZ.PQPCLR_HORA as
  voracode          number;
  vIDProc           number;
  verrmsg           varchar2(200);
  voraerr           varchar2(200);
  vretc             varchar2(100);
  vpaso             varchar2(10);
  verrcod           varchar2(2);
  efinerror         exception;
  vemisor           char(4);
  vlote             char(3);
  vcod_hrcierre     char(1);
  videmen_p00_accc  number;
  vcodfun_p24_accc  varchar2(3);
  videest_p42_accc  varchar2(15);
  vlondat_p48_accc  varchar2(3);
  vcodr14_p48_accc  varchar2(2);
  vctaabo_p48_accc  varchar2(20);
  vnumanc_s77_accc  number;
  vnumser_s80_accc  number;
  vnumaut_s81_accc  number;
  vcobpag_s85_accc  number;
  vimpanc_s89_accc  number;
  videdes_l93_accc  number;
  videdes_s93_accc  varchar2(6);
  videori_l94_accc  number;
  videori_s94_accc  varchar2(11);
  videter_p41_accc  varchar2(8);
  vcombol_p48_accc  varchar2(12);
  vfiller_pxxx_accc varchar2(587);
  vmoneda           char(3)        := '862';
  vfecha            char(6);
  padqui            char(6);
  vstr1             varchar2(2):='';
  vstr2             varchar2(2):='';

PROCEDURE pp_main (pfecha DATE, pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq CHAR) IS
BEGIN

    /* ID DE PROCESO
    ****************/
    vPaso   :='paso 01';
    vIDProc := pqmonproc.InsMonProc ('PCLRHR');

     /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'EntAdq: '||pcod_entadq||' | FecSes: '||TO_CHAR(pfecha,'YYYY-MM-DD')||' | HCierre: '||pcod_hrcierre||' | HProc: '||phra_proceso);
    pqmonproc.inslog(vIDProc, 'M', 'Inicia Proceso Cuadre por Hora');

    vmoneda := PQCOMERCIOS.GCW_F_GETMONEDAVIG(pfecha);
    pp_clr_merchant_hora(pfecha, pcod_hrcierre, phra_proceso, pcod_entadq);
    pp_clr_emisor_hora  (pfecha, pcod_hrcierre, phra_proceso, pcod_entadq);

    commit;

    pqpclr_platco_hora.pp_clearing_main (pfecha,pcod_hrcierre,phra_proceso, pcod_entadq);
    /* FIN DE PROCESO
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'Fin OK Proceso Cuadre por Hora');
    vRetC  := pqmonproc.updmonproc (vIDProc, 'F');

END;

PROCEDURE pp_clr_merchant_hora (pfecha DATE, pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq CHAR) IS

    vmontneto         vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmd               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmc               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vma               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmac              vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmcc              vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vcd               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcc               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vca               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcc_vcac          vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcd_vca_vccc      vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcac              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vccc              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcpa              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcco              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vSIGNO            vSIGNO_ARRAY:=vSIGNO_ARRAY('C','C','C');
    vnumtotal         vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnd               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnc               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vna               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnac              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vncc              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnpa              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnco              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);

    PROCEDURE clean_variables IS
    BEGIN
        vmd(1) := 0; vmd(2) := 0; vmd(3) := 0;
        vmc(1) := 0; vmc(2) := 0; vmc(3) := 0;
        vma(1) := 0; vma(2) := 0; vma(3) := 0;
        vmac(1):= 0; vmac(2):= 0; vmac(3):= 0;
        vmcc(1):= 0; vmcc(2):= 0; vmcc(3):= 0;
        vcd(1) := 0; vcd(2) := 0; vcd(3) := 0;
        vcc(1) := 0; vcc(2) := 0; vcc(3) := 0;
        vca(1) := 0; vca(2) := 0; vca(3) := 0;
        vcac(1):= 0; vcac(2):= 0; vcac(3):= 0;
        vccc(1):= 0; vccc(2):= 0; vccc(3):= 0;
        vcpa(1):= 0; vcpa(2):= 0; vcpa(3):= 0;
        vcco(1):= 0; vcco(2):= 0; vcco(3):= 0;
        vnd(1) := 0; vnd(2) := 0; vnd(3) := 0;
        vnc(1) := 0; vnc(2) := 0; vnc(3) := 0;
        vna(1) := 0; vna(2) := 0; vna(3) := 0;
        vnac(1):= 0; vnac(2):= 0; vnac(3):= 0;
        vncc(1):= 0; vncc(2):= 0; vncc(3):= 0;
        vnpa(1):= 0; vnpa(2):= 0; vnpa(3):= 0;
        vnco(1):= 0; vnco(2):= 0; vnco(3):= 0;
        vcc_vcac(1) := 0; vcc_vcac(2) := 0; vcc_vcac(3) := 0;
        vcd_vca_vccc(1) := 0; vcd_vca_vccc(2) := 0; vcd_vca_vccc(3) := 0;
    END;

    PROCEDURE clr_merchant_det (plote char, parchivo1 char, parchivo2 char,pIdedes_s93_accc char,pIdeori_s94_accc char) is
        vEntidad varchar2(4);
        vEntidadCP varchar2(4);
    BEGIN
        clean_variables ;
        vlote             := plote;
        vIdedes_s93_accc  := pIdedes_s93_accc;
        vIdeori_s94_accc  := pIdeori_s94_accc;
        vEntidad := substr(pIdedes_s93_accc,3,4);
        vEntidadCP := substr(pIdedes_s93_accc,3,4);

        IF    vlote = '311' AND substr(vEntidad,3,2) = '99' THEN vEntidad := '0108' ;
        ELSIF vlote = '311' AND substr(vEntidad,3,2) = '98' THEN vEntidad := '0105' ;
        ELSIF vlote = '312' AND substr(vEntidad,1,2) = '99' THEN vEntidad := '8001' ;
        ELSIF vlote = '312' AND substr(vEntidad,1,2) = '98' THEN vEntidad := '8010' ;
        ELSIF vlote = '314' THEN vEntidad := '8001' ;
        ELSIF vlote = '316' THEN vEntidad := '8010' ;
        END IF;

        pqmonproc.InsLog(vIDProc, 'M', 'inicia clr_merchant lote '||vlote||' entidad '||vEntidad);

        -- Operaciones de Compra
        get_compra_m(parchivo1,phra_proceso,1244,'0','200000',vlote,vmoneda,vfecha,pcod_hrcierre,padqui,vmc,vcc,vnc);
        -- Operaciones de Devolucion
        get_devolucion_m(parchivo1,phra_proceso,1244,'200000','300000',vlote,vmoneda,vfecha,pcod_hrcierre,padqui,vmd,vcd, vnd);
        -- Operaciones de Anulacion de Compra
        get_anulacompra_m(parchivo1,phra_proceso,1444,'0','200000',vlote,vmoneda,vfecha,pcod_hrcierre,padqui,vma,vca,vna);

        IF parchivo2 <> 'X' AND vmoneda <> '840' THEN
            pqmonproc.inslog(vIDProc, 'M', 'CP Merchant '||parchivo2||' '||vlote||' '||vEntidad||' '||vfecha||' '||pcod_hrcierre||' '||padqui||' '||vemisor);
            -- operaciones miscelaneas de cobro
            get_copa_m(parchivo2,phra_proceso,1744,'700','D','701','D',vlote,vEntidadCP,vfecha,pcod_hrcierre,padqui,vcco, vnco);
            -- operaciones miscelaneas de pago
            get_copa_m(parchivo2,phra_proceso,1744,'700','C','701','C',vlote,vEntidadCP,vfecha,pcod_hrcierre,padqui,vcpa, vnpa);
            pqmonproc.inslog(vIDProc, 'M', 'CP Merchant '||to_char(vnco(1))||' '||to_char(vnpa(1)));
        END IF;

        -- CREDITO
        vmontneto(1)    := vmd(1) - vmc(1) + vma(1) - vmac(1) + vmcc(1) - vcd(1) + vcc(1) - vca(1) + vcac(1) - vccc(1) + vcpa(1) - vcco(1);
        vnumtotal(1)    := vnd(1) + vnc(1) + vna(1) + vnac(1) + vncc(1) + vnpa(1) + vnco(1);
        vcc_vcac(1)     := vcc(1) + vcac(1);
        vcd_vca_vccc(1) := vcd(1) + vca(1) + vccc(1);
        IF vmontneto(1) >= 0 THEN
            vSIGNO(1) := 'D';
        ELSE
            vSIGNO(1) := 'C';
            vmontneto(1) := vmontneto(1) * -1;
        END IF;

        -- DEBITO
        vmontneto(2)    := vmd(2) - vmc(2) + vma(2) - vmac(2) + vmcc(2) - vcd(2) + vcc(2) - vca(2) + vcac(2) - vccc(2) + vcpa(2) - vcco(2);
        vnumtotal(2)    := vnd(2) + vnc(2) + vna(2) + vnac(2) + vncc(2) + vnpa(2) + vnco(2);
        vcc_vcac(2)     := vcc(2) + vcac(2);
        vcd_vca_vccc(2) := vcd(2) + vca(2) + vccc(2);
        IF vmontneto(2) >= 0 THEN
            vSIGNO(2) := 'D';
        ELSE
            vSIGNO(2) := 'C';
            vmontneto(2) := vmontneto(2) * -1;
        END IF;

        -- ALIMENTACION
        vmontneto(3)    := vmd(3) - vmc(3) + vma(3) - vmac(3) + vmcc(3) - vcd(3) + vcc(3) - vca(3) + vcac(3) - vccc(3) + vcpa(3) - vcco(3);
        vnumtotal(3)    := vnd(3) + vnc(3) + vna(3) + vnac(3) + vncc(3) + vnpa(3) + vnco(3);
        vcc_vcac(3)     := vcc(3) + vcac(3);
        vcd_vca_vccc(3) := vcd(3) + vca(3) + vccc(3);
        IF vmontneto(3) >= 0 THEN
            vSIGNO(3) := 'D';
        ELSE
            vSIGNO(3) := 'C';
            vmontneto(3) := vmontneto(3) * -1;
        END IF;

        pp_insert_clrlmtota_hora(vcod_hrcierre,     phra_proceso,     videmen_p00_accc , vfecha,
                                 vCodfun_p24_accc,                    vlote,
                                 vIdeest_p42_accc, vLondat_p48_accc,  vCodr14_p48_accc,
                                 vCtaabo_p48_accc, vnumtotal,         vnd,
                                 vna,              vnc,               vNumanc_s77_accc,
                                 vNumser_s80_accc, vNumaut_s81_accc,  vCobpag_s85_accc,
                                 vmd,              vma,               vmc,
                                 vImpanc_s89_accc, vIdedes_l93_accc,  vIdedes_s93_accc,
                                 vIdeori_l94_accc, vIdeori_s94_accc,  vSIGNO,
                                 vmontneto,        vmac,              vmcc,
                                 vnac,             vncc,              vcc_vcac,
                                 vcd_vca_vccc,     videter_p41_accc,  vcombol_p48_accc,
                                 vnpa,             vnco,              vcpa,
                                 vcco,             vmoneda,           vfiller_pxxx_accc,
                                 pcod_entadq);

    END;

BEGIN

    -- Validacion de la Hora de Cierre
    IF pcod_hrcierre NOT IN ('1','2') THEN
        verrmsg:='fin error | hora de cierre incorrecta [1=00:00 horas/2=04:00 horas]';
        verrcod:= '1';
        RAISE efinerror;
    END IF;

    vfecha            := to_char(pfecha,'YYMMDD');
    vcod_hrcierre     := pcod_hrcierre;
    padqui            := pcod_entadq;
    videmen_p00_accc  := '1544';
    vCodfun_p24_accc  := '500';
    vIdeest_p42_accc  := NULL;
    vLondat_p48_accc  := NULL;
    vCodr14_p48_accc  := NULL;
    vCtaabo_p48_accc  := NULL;
    vNumanc_s77_accc  := 0;
    vNumser_s80_accc  := 0;
    vNumaut_s81_accc  := 0;
    vCobpag_s85_accc  := 0;
    vImpanc_s89_accc  := 0;
    vIdedes_l93_accc  := 0;
    vIdeori_l94_accc  := 6;
    videter_p41_accc  := NULL;
    vcombol_p48_accc  := NULL;
    vfiller_pxxx_accc := NULL;

    DELETE CLR_LMTOTA_HORA
     WHERE SESION_P28_ACCC = vfecha
       AND COD_ENTADQ = pcod_entadq
       AND COD_HRCIERRE = pcod_hrcierre
       AND hra_proceso = phra_proceso
       AND MONEDA_PXXX_ACCC = vmoneda;

    -- Inicio Proceso Principal Merchant
    --- Lote 311: Nacionales Platco
    --- Lote 312: Nacionales NO Platco
    --- Lote 314: VISA Internacional
    --- Lote 316  MC Internacional

    -- Presentaciones y Representaciones

    vPaso   :='paso 02a';
    clr_merchant_det ('311', 'clr_mx0105', 'X', '990105','990105');

    vPaso   :='paso 02b';
    clr_merchant_det ('312', 'clr_mx0105', 'X', '990105','990105');

    vPaso   :='paso 02c';
    clr_merchant_det ('314', 'clr_mx0105', 'X', '990105','990105');

    vPaso   :='paso 02d';
    clr_merchant_det ('316', 'clr_mx0105', 'X', '990105','990105');

    vPaso   :='paso 03a';
    clr_merchant_det ('311', 'clr_mx0108', 'X', '990108','990108');

    vPaso   :='paso 03b';
    clr_merchant_det ('312', 'clr_mx0108', 'X', '990108','990108');

    vPaso   :='paso 03c';
    clr_merchant_det ('314', 'clr_mx0108', 'X', '990108','990108');

    vPaso   :='paso 03d';
    clr_merchant_det ('316', 'clr_mx0108', 'X', '990108','990108');


    --Ventas, devoluciones, anulaciones, cobros-pagos, resolucion de rechazos-retornos (ventas)
    -- Lote 311
    vPaso   :='paso 04a';
    clr_merchant_det ('311', 'clr_mx9998', 'clr_copam0105', '999998','998000');
    vPaso   :='paso 04b';
    clr_merchant_det ('311', 'clr_mx9898', 'clr_copam0105', '999898','998000');
    vPaso   :='paso 04c';
    clr_merchant_det ('311', 'clr_mx9798', 'X', '999798','998000');
    vPaso   :='paso 04d';
    clr_merchant_det ('311', 'clr_mx8998', 'X', '998998','998000');
    vPaso   :='paso 04e';
    clr_merchant_det ('311', 'clr_mx9999', 'clr_copam0108', '999999','998000');
    vPaso   :='paso 04f';
    clr_merchant_det ('311', 'clr_mx9899', 'clr_copam0108', '999899','998000');
    vPaso   :='paso 04g';
    clr_merchant_det ('311', 'clr_mx8999', 'X', '998999','998000');

            --INICIO IPR 1068
            --FECHA DE CREACION : 26/10/2012
            --AUTOR : EDGAR MENA AZA¿ERO.
            --DESCRIPCION : Permite agregar Diners para BP.

            vPaso := 'paso 04h';
            clr_merchant_det ('311', 'clr_mx9799', 'X', '999799', '998000');
            --FIN IPR 1068

    -- Lote 312
    vPaso   :='paso 05a';
    clr_merchant_det ('312', 'clr_mx9998', 'clr_copam0105', '999998','998000');
    vPaso   :='paso 05b';
    clr_merchant_det ('312', 'clr_mx9898', 'clr_copam0105', '999898','998000');
    vPaso   :='paso 05c';
    clr_merchant_det ('312', 'clr_mx9798', 'X', '999798','998000');
    vPaso   :='paso 05d';
    clr_merchant_det ('312', 'clr_mx8998', 'X', '998998','998000');
    vPaso   :='paso 05e';
    clr_merchant_det ('312', 'clr_mx9999', 'clr_copam0108', '999999','998000');
    vPaso   :='paso 05f';
    clr_merchant_det ('312', 'clr_mx9899', 'clr_copam0108', '999899','998000');
    vPaso   :='paso 05g';
    clr_merchant_det ('312', 'clr_mx8999', 'X', '998999','998000');

            --INICIO IPR 1068
            --FECHA DE CREACION : 26/10/2012
            --AUTOR : EDGAR MENA AZA¿ERO.
            --DESCRIPCION : Permite agregar Diners para BP.

            vPaso := 'paso 05h';
            clr_merchant_det ('312', 'clr_mx9799', 'X', '999799', '998000');
            --FIN IPR 1068

    -- Lote 314
    vPaso   :='paso 06a';
    clr_merchant_det ('314', 'clr_mx9998', 'clr_copam0105', '999998','998000');
    vPaso   :='paso 06b';
    clr_merchant_det ('314', 'clr_mx9898', 'X', '999898','998000');
    vPaso   :='paso 06c';
    clr_merchant_det ('314', 'clr_mx9798', 'X', '999798','998000');
    vPaso   :='paso 06d';
    clr_merchant_det ('314', 'clr_mx8998', 'X', '998998','998000');
    vPaso   :='paso 06e';
    clr_merchant_det ('314', 'clr_mx9999', 'clr_copam0108', '999999','998000');
    vPaso   :='paso 06f';
    clr_merchant_det ('314', 'clr_mx9899', 'X', '999899','998000');
    vPaso   :='paso 06g';
    clr_merchant_det ('314', 'clr_mx8999', 'X', '998999','998000');

            --INICIO IPR 1068
            --FECHA DE CREACION : 26/10/2012
            --AUTOR : EDGAR MENA AZA¿ERO.
            --DESCRIPCION : Permite agregar Diners para BP.

            vPaso := 'paso 06h';
            clr_merchant_det ('314', 'clr_mx9799', 'X', '999799', '998000');
            --FIN IPR 1068

    -- Lote 316
    vPaso   :='paso 07a';
    clr_merchant_det ('316', 'clr_mx9998', 'clr_copam0105', '999998','998000');
    vPaso   :='paso 07b';
    clr_merchant_det ('316', 'clr_mx9898', 'X', '999898','998000');
    vPaso   :='paso 07c';
    clr_merchant_det ('316', 'clr_mx9798', 'X', '999798','998000');
    vPaso   :='paso 07d';
    clr_merchant_det ('316', 'clr_mx8998', 'X', '998998','998000');
    vPaso   :='paso 07e';
    clr_merchant_det ('316', 'clr_mx9999', 'clr_copam0108', '999999','998000');
    vPaso   :='paso 07f';
    clr_merchant_det ('316', 'clr_mx9899', 'X', '999899','998000');
    vPaso   :='paso 07g';
    clr_merchant_det ('316', 'clr_mx8999', 'X', '998999','998000');

 --INICIO IPR 1068
            --FECHA DE CREACION : 26/10/2012
            --AUTOR : EDGAR MENA AZA¿ERO.
            --DESCRIPCION : Permite agregar Diners para BP.

            vPaso := 'paso 07h';
            clr_merchant_det ('316', 'clr_mx9799', 'X', '999799', '998000');
            --FIN IPR 1068
EXCEPTION
    WHEN efinerror THEN
         pqmonproc.inslog(vIDProc, 'E', verrmsg);
         vretc :=pqmonproc.updmonproc (vIDProc, 'E', verrcod);
         RAISE_APPLICATION_ERROR (-20000, verrmsg);
    WHEN OTHERS THEN
         voracode:=SQLCODE;
         voraerr:=SUBSTR(SQLERRM,1,200);
         pqmonproc.inslog(vIDProc, 'E', voraerr);
         vretc :=pqmonproc.updmonproc (vIDProc, 'E', '99');
         DBMS_OUTPUT.PUT_LINE('fin error vretc: '||vretc);
         DBMS_OUTPUT.PUT_LINE('fin error voraerr: '||voraerr);
         RAISE_APPLICATION_ERROR (-20001, vpaso||'-'||voraerr);
END;


PROCEDURE pp_clr_emisor_hora (pfecha DATE,pcod_hrcierre CHAR, phra_proceso CHAR, pcod_entadq char) IS

    vmontneto         vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmd               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmc               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vma               vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmac              vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vmcc              vP04OUT_ARRAY:=vP04OUT_ARRAY(0,0,0);
    vcd               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcc               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vca               vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcc_vcac          vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcd_vca_vccc      vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcac              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vccc              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcpa              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vcco              vP46OUT_ARRAY:=vP46OUT_ARRAY(0,0,0);
    vSIGNO            vSIGNO_ARRAY:=vSIGNO_ARRAY('C','C','C');
    vnumtotal         vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnd               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnc               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vna               vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnac              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vncc              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnpa              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);
    vnco              vNUMREG_ARRAY:=vNUMREG_ARRAY(0,0,0);

    CURSOR c_emisor IS
    SELECT cod_emisor
      FROM clr_emisores
     WHERE cod_emisor NOT IN ('0105', '0108');


    CURSOR c_emisor_in IS
    SELECT cod_emisor
      FROM clr_emisores
     WHERE cod_emisor IN ('0105', '0108');

    PROCEDURE clean_variables is
    BEGIN
        vmd(1) := 0; vmd(2) := 0; vmd(3) := 0;
        vmc(1) := 0; vmc(2) := 0; vmc(3) := 0;
        vma(1) := 0; vma(2) := 0; vma(3) := 0;
        vmac(1):= 0; vmac(2):= 0; vmac(3):= 0;
        vmcc(1):= 0; vmcc(2):= 0; vmcc(3):= 0;
        vcd(1) := 0; vcd(2) := 0; vcd(3) := 0;
        vcc(1) := 0; vcc(2) := 0; vcc(3) := 0;
        vca(1) := 0; vca(2) := 0; vca(3) := 0;
        vcac(1):= 0; vcac(2):= 0; vcac(3):= 0;
        vccc(1):= 0; vccc(2):= 0; vccc(3):= 0;
        vcpa(1):= 0; vcpa(2):= 0; vcpa(3):= 0;
        vcco(1):= 0; vcco(2):= 0; vcco(3):= 0;
        vnd(1) := 0; vnd(2) := 0; vnd(3) := 0;
        vnc(1) := 0; vnc(2) := 0; vnc(3) := 0;
        vna(1) := 0; vna(2) := 0; vna(3) := 0;
        vnac(1):= 0; vnac(2):= 0; vnac(3):= 0;
        vncc(1):= 0; vncc(2):= 0; vncc(3):= 0;
        vnpa(1):= 0; vnpa(2):= 0; vnpa(3):= 0;
        vnco(1):= 0; vnco(2):= 0; vnco(3):= 0;
        vcc_vcac(1) := 0; vcc_vcac(2) := 0; vcc_vcac(3) := 0;
        vcd_vca_vccc(1) := 0; vcd_vca_vccc(2) := 0; vcd_vca_vccc(3) := 0;
    END;

    PROCEDURE clr_emisor_det (plote char, parchivo1 char, parchivo2 char, pIdedes_s93_accc char,pIdeori_s94_accc char) is
    BEGIN
        clean_variables ;
        vlote             := plote;
        vIdedes_s93_accc  := pIdedes_s93_accc;
        vIdeori_s94_accc  := pIdeori_s94_accc;

        pqmonproc.InsLog(vIDProc, 'M', 'inicia clr_emisor lote '||vlote||' entidad '||pArchivo1);

        -- operaciones de compra (original)
        get_compra_e(parchivo1,phra_proceso,1240,'0','200000',vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,padqui,vmc,vcc,vnc);
        -- operaciones de devolucion
        get_devolucion_e(parchivo1,phra_proceso,1240,'200000','300000',vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,padqui,vmd,vcd,vnd);
        -- operaciones de anulacion de compra
        get_anulacompra_e(parchivo1,phra_proceso,1440,'0','200000',vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,padqui,vma,vca,vna);

        IF parchivo2 <> 'X' AND vmoneda <> '840' THEN
            pqmonproc.inslog(vIDProc, 'M', 'CP Emisor '||vlote||' '||vemisor||padqui);
            -- operaciones miscelaneas de cobro
            get_cobro_e(parchivo2,phra_proceso,1740,'700','D','701','D',vlote,vemisor,vfecha,pcod_hrcierre,padqui,vcco, vnco);
            -- operaciones miscelaneas de pago
            get_pago_e(parchivo2,phra_proceso,1740,'700','C','701','C',vlote,vemisor,vfecha,pcod_hrcierre,padqui,vcpa, vnpa);
            pqmonproc.inslog(vIDProc, 'M', 'CP Emisor '||to_char(vnco(1))||' '||to_char(vnpa(1)));
        END IF;

        -- CREDITO
        vmontneto(1)    := vmd(1) - vmc(1) + vma(1) - vmac(1) + vmcc(1) - vcd(1)  + vcc(1) - vca(1) + vcac(1) - vccc(1) + vcpa(1) - vcco(1);
        vnumtotal(1)    := vnd(1) + vnc(1) + vna(1) + vnac(1) + vncc(1) + vnco(1) + vnpa(1);
        vcc_vcac(1)        := vcc(1) + vcac(1);
        vcd_vca_vccc(1)    := vcd(1) + vca(1) + vccc(1);

        IF vmontneto(1) >= 0 THEN
            vSIGNO(1) := 'C';
        ELSE
            vSIGNO(1) := 'D';
            vmontneto(1) := vmontneto(1) * -1;
        END IF;

        -- DEBITO
        vmontneto(2)    := vmd(2) - vmc(2) + vma(2) - vmac(2) + vmcc(2) - vcd(2)  + vcc(2) - vca(2) + vcac(2) - vccc(2) + vcpa(2) - vcco(2);
        vnumtotal(2)    := vnd(2) + vnc(2) + vna(2) + vnac(2) + vncc(2) + vnco(2) + vnpa(2);
        vcc_vcac(2)        := vcc(2) + vcac(2);
        vcd_vca_vccc(2)    := vcd(2) + vca(2) + vccc(2);

        IF vmontneto(2) >= 0 THEN
            vSIGNO(2) := 'C';
        ELSE
            vSIGNO(2) := 'D';
            vmontneto(2) := vmontneto(2) * -1;
        END IF;

        -- ALIMENTACION
        vmontneto(3)    := vmd(3) - vmc(3) + vma(3) - vmac(3) + vmcc(3) - vcd(3)  + vcc(3) - vca(3) + vcac(3) - vccc(3) + vcpa(3) - vcco(3);
        vnumtotal(3)    := vnd(3) + vnc(3) + vna(3) + vnac(3) + vncc(3) + vnco(3) + vnpa(3);
        vcc_vcac(3)        := vcc(3) + vcac(3);
        vcd_vca_vccc(3)    := vcd(3) + vca(3) + vccc(3);

        IF vmontneto(3) >= 0 THEN
            vSIGNO(3) := 'C';
        ELSE
            vSIGNO(3) := 'D';
            vmontneto(3) := vmontneto(3) * -1;
        END IF;

        PP_INSERT_CLRLETOTA_HORA(
                        pcod_hrcierre,      phra_proceso,      videmen_p00_accc ,
                        vfecha,             vCodfun_p24_accc,  vlote,
                        vIdeest_p42_accc,   vLondat_p48_accc,  vCodr14_p48_accc,
                        vCtaabo_p48_accc,   vnumtotal,         vnd,
                        vna,                vnc,               vNumanc_s77_accc,
                        vNumser_s80_accc,   vNumaut_s81_accc,  vCobpag_s85_accc,
                        vmd,                vma,               vmc,
                        vImpanc_s89_accc,   vIdedes_l93_accc,  vIdedes_s93_accc,
                        vIdeori_l94_accc,   vIdeori_s94_accc,  vSIGNO,
                        vmontneto,          vmac,              vmcc,
                        vnac,               vncc,              vcc_vcac,
                        vcd_vca_vccc,       videter_p41_accc,  vcombol_p48_accc,
                        vnpa,               vnco,              vcpa,
                        vcco,               vmoneda,           vfiller_pxxx_accc,
                        pcod_entadq);

    END;

BEGIN

    vemisor         := null;
    vfecha          := to_char(pfecha,'YYMMDD');
    vcod_hrcierre   := pcod_hrcierre;
    padqui          := pcod_entadq;

    DELETE clr_letota_hora
     WHERE sesion_p28_accc = vfecha
       AND cod_entadq = pcod_entadq
       AND cod_hrcierre = pcod_hrcierre
       AND hra_proceso = phra_proceso
       AND moneda_pxxx_accc = vmoneda;

    videmen_p00_accc  := '1544';
    vCodfun_p24_accc  := '500';
    vIdeest_p42_accc  := NULL;
    vLondat_p48_accc  := NULL;
    vCodr14_p48_accc  := NULL;
    vCtaabo_p48_accc  := NULL;
    vNumanc_s77_accc  := 0;
    vNumser_s80_accc  := 0;
    vNumaut_s81_accc  := 0;
    vCobpag_s85_accc  := 0;
    vImpanc_s89_accc  := 0;
    vIdedes_l93_accc  := 6;
    vIdeori_l94_accc  := 6;
    videter_p41_accc  := NULL;
    vcombol_p48_accc  := NULL;
    vfiller_pxxx_accc := NULL;

    vPaso   :='paso 08';
    FOR reg IN c_emisor_in LOOP
        vemisor           := reg.cod_emisor;
        clr_emisor_det ('511', 'clr_ex'||vemisor, 'clr_copae'||vemisor, '99'||vemisor,'998000');
    END LOOP;

    FOR reg IN c_emisor LOOP
      vemisor           := reg.cod_emisor;
      clr_emisor_det ('512', 'clr_ex'||vemisor, 'clr_copae'||vemisor, '99'||vemisor,'998000');
      clr_emisor_det ('514', 'clr_ex'||vemisor, 'clr_copae'||vemisor, '99'||vemisor,'998000');
      clr_emisor_det ('516', 'clr_ex'||vemisor, 'clr_copae'||vemisor, '99'||vemisor,'998000');
    END LOOP;

EXCEPTION
    WHEN efinerror THEN
         pqmonproc.inslog(vIDProc, 'E', verrmsg);
         vretc :=pqmonproc.updmonproc (vIDProc, 'E', verrcod);
         RAISE_APPLICATION_ERROR (-20000, verrmsg);
    WHEN OTHERS THEN
         voracode:=SQLCODE;
         voraerr:=SUBSTR(SQLERRM,1,200);
         pqmonproc.inslog(vIDProc, 'E', voraerr);
         vretc :=pqmonproc.updmonproc (vIDProc, 'E', '99');
         DBMS_OUTPUT.PUT_LINE('fin error vretc: '||vretc);
         DBMS_OUTPUT.PUT_LINE('fin error voraerr: '||voraerr);
         RAISE_APPLICATION_ERROR (-20001, vpaso||'-'||voraerr);
END;

PROCEDURE pp_insert_clrlmtota_hora(
                              vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char)
AS
BEGIN
  -- CREDITO
  IF vnumtotal(1) > 0 THEN
     INSERT INTO CLR_LMTOTA_HORA
       (cod_hrcierre,
        hra_proceso,       idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,   codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,   ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,   ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,   numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,   numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,   impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,   impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,   ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,   netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,  nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,  cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,   nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,  carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,  cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(1),
        vnd(1),             vna(1),                  vnc(1),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(1),                  vma(1),
        vmc(1),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(1),          vmontneto(1),            vmac(1),
        vmcc(1),            vnac(1),                 vncc(1),
        vcc_vcac(1),        vcd_vca_vccc(1),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(1),                 vnco(1),
        vcpa(1),            vcco(1),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'c');
  END IF;
  -- DEBITO
  IF vnumtotal(2) > 0 THEN
     INSERT INTO CLR_LMTOTA_HORA
       (cod_hrcierre,
        hra_proceso,       idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,   codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,   ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,   ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,   numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,   numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,   impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,   impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,   ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,   netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,  nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,  cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,   nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,  carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,  cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(2),
        vnd(2),             vna(2),                  vnc(2),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(2),                  vma(2),
        vmc(2),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(2),          vmontneto(2),            vmac(2),
        vmcc(2),            vnac(2),                 vncc(2),
        vcc_vcac(2),        vcd_vca_vccc(2),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(2),                 vnco(2),
        vcpa(2),            vcco(2),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'd');
  END IF;
  -- ALIMENTACION
  IF vnumtotal(3) > 0 THEN
     INSERT INTO CLR_LMTOTA_HORA
       (cod_hrcierre,
        hra_proceso,       idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,   codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,   ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,   ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,   numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,   numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,   impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,   impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,   ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,   netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,  nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,  cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,   nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,  carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,  cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(3),
        vnd(3),             vna(3),                  vnc(3),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(3),                  vma(3),
        vmc(3),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(3),          vmontneto(3),            vmac(3),
        vmcc(3),            vnac(3),                 vncc(3),
        vcc_vcac(3),        vcd_vca_vccc(3),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(3),                 vnco(3),
        vcpa(3),            vcco(3),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'a');
  END IF;
  COMMIT;
END;

PROCEDURE pp_insert_clrlmtota_hora_misc (
                                    vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                                    vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                                    vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                                    vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                                    vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                                    vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                                    vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                                    vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                                    vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                                    vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                                    vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                                    vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                                    vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                                    vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                                    pcod_entadq char)
AS
BEGIN
  -- MISCELANEOS
  IF vnumtotal(1) > 0 THEN
     INSERT INTO CLR_LMTOTA_HORA
       (cod_hrcierre,
        hra_proceso,       idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,   codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,   ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,   ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,   numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,   numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,   impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,   impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,   ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,   netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,  nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,  cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,   nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,  carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,  cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(1),
        vnd(1),             vna(1),                  vnc(1),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(1),                  vma(1),
        vmc(1),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(1),          vmontneto(1),            vmac(1),
        vmcc(1),            vnac(1),                 vncc(1),
        vcc_vcac(1),        vcd_vca_vccc(1),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(1),                 vnco(1),
        vcpa(1),            vcco(1),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'm');
  END IF;
  COMMIT;
END;

PROCEDURE pp_insert_clrletota_hora(
                              vcod_hrcierre CHAR,           phra_proceso CHAR,          videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char)
AS
BEGIN
  -- CREDITO
  IF vnumtotal(1) > 0 THEN
     INSERT INTO CLR_LETOTA_HORA
       (cod_hrcierre,
        hra_proceso,        idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,    codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,    ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,    ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,    numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,    numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,    impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,    impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,    ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,    netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,   nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,   cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,    nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,   carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,   cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(1),
        vnd(1),             vna(1),                  vnc(1),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(1),                  vma(1),
        vmc(1),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(1),          vmontneto(1),            vmac(1),
        vmcc(1),            vnac(1),                 vncc(1),
        vcc_vcac(1),        vcd_vca_vccc(1),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(1),                 vnco(1),
        vcpa(1),            vcco(1),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'c');
  END IF;
  -- DEBITO
  IF vnumtotal(2) > 0 THEN
     INSERT INTO CLR_LETOTA_HORA
       (cod_hrcierre,
        hra_proceso,        idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,    codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,    ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,    ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,    numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,    numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,    impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,    impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,    ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,    netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,   nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,   cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,    nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,   carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,   cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(2),
        vnd(2),             vna(2),                  vnc(2),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(2),                  vma(2),
        vmc(2),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(2),          vmontneto(2),            vmac(2),
        vmcc(2),            vnac(2),                 vncc(2),
        vcc_vcac(2),        vcd_vca_vccc(2),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(2),                 vnco(2),
        vcpa(2),            vcco(2),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'd');
  END IF;
  -- ALIMENTACION
  IF vnumtotal(3) > 0 THEN
     INSERT INTO CLR_LETOTA_HORA
       (cod_hrcierre,
        hra_proceso,        idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,    codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,    ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,    ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,    numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,    numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,    impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,    impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,    ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,    netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,   nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,   cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,    nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,   carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,   cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(3),
        vnd(3),             vna(3),                  vnc(3),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(3),                  vma(3),
        vmc(3),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(3),          vmontneto(3),            vmac(3),
        vmcc(3),            vnac(3),                 vncc(3),
        vcc_vcac(3),        vcd_vca_vccc(3),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(3),                 vnco(3),
        vcpa(3),            vcco(3),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'a');
  END IF;
  COMMIT;
END;

PROCEDURE pp_insert_clrletota_hora_misc (
                              vcod_hrcierre CHAR,           phra_proceso CHAR,           videmen_p00_accc NUMBER,
                              vfecha VARCHAR2,              vCodfun_p24_accc VARCHAR2,  vlote VARCHAR2,
                              vIdeest_p42_accc VARCHAR2,    vLondat_p48_accc VARCHAR2,  vCodr14_p48_accc VARCHAR2,
                              vCtaabo_p48_accc VARCHAR2,    vnumtotal vNUMREG_ARRAY,    vnd vNUMREG_ARRAY,
                              vna vNUMREG_ARRAY,            vnc vNUMREG_ARRAY,          vNumanc_s77 NUMBER,
                              vNumser_s80_accc NUMBER,      vNumaut_s81_accc NUMBER,    vCobpag_s85_accc NUMBER,
                              vmd vP04OUT_ARRAY,            vma vP04OUT_ARRAY,          vmc vP04OUT_ARRAY,
                              vImpanc_s89_accc NUMBER,      vIdedes_l93_accc NUMBER,    vIdedes_s93_accc VARCHAR2,
                              vIdeori_l94_accc NUMBER,      vIdeori_s94_accc VARCHAR2,  vSIGNO vSIGNO_ARRAY,
                              vmontneto vP04OUT_ARRAY,      vmac vP04OUT_ARRAY,         vmcc vP04OUT_ARRAY,
                              vnac vNUMREG_ARRAY,           vncc vNUMREG_ARRAY,         vcc_vcac vP46OUT_ARRAY,
                              vcd_vca_vccc vP46OUT_ARRAY,   videter_p41_accc VARCHAR2,  vcombol_p48_accc VARCHAR2,
                              vnpa vNUMREG_ARRAY,           vnco vNUMREG_ARRAY,         vcpa vP46OUT_ARRAY,
                              vcco vP46OUT_ARRAY,           vmoneda VARCHAR2,           vfiller_pxxx_accc VARCHAR2,
                              pcod_entadq char)
AS
BEGIN
  -- MISCELANEOS
  IF vnumtotal(1) > 0 THEN
     INSERT INTO CLR_LETOTA_HORA
       (cod_hrcierre,
        hra_proceso,        idemen_p00_accc,         idetra_p11_accc,
        timloc_p12_accc,    codfun_p24_accc,         sesion_p28_accc,
        inlote_p29_accc,    ideest_p42_accc,         londat_p48_accc,
        codr14_p48_accc,    ctaabo_p48_accc,         nummen_s71_accc,
        numabo_s74_accc,    numana_s75_accc,         numcar_s76_accc,
        numanc_s77_accc,    numser_s80_accc,         numaut_s81_accc,
        cobpag_s85_accc,    impabo_s86_accc,         impana_s87_accc,
        impcar_s88_accc,    impanc_s89_accc,         idedes_l93_accc,
        idedes_s93_accc,    ideori_l94_accc,         ideori_s94_accc,
        signox_s97_accc,    netoxx_s97_accc,         ichaab_s105_accc,
        ichaca_s106_accc,   nchaab_s107_accc,        nchaca_s108_accc,
        cuoabo_s109_accc,   cuocar_s110_accc,        ideter_p41_accc,
        combol_p48_accc,    nabomis_s111_accc,       ncarmis_s112_accc,
        abomis_s113_accc,   carmis_s114_accc,        moneda_pxxx_accc,
        filler_pxxx_accc,   cod_entadq,              tipmov)
     VALUES
       (vcod_hrcierre,
        phra_proceso,       videmen_p00_accc,        seq_clr_totales_hora.NEXTVAL,
        vfecha || '000000', vCodfun_p24_accc,        vfecha,
        vlote,              vIdeest_p42_accc,        vLondat_p48_accc,
        vCodr14_p48_accc,   vCtaabo_p48_accc,        vnumtotal(1),
        vnd(1),             vna(1),                  vnc(1),
        vNumanc_s77,        vNumser_s80_accc,        vNumaut_s81_accc,
        vCobpag_s85_accc,   vmd(1),                  vma(1),
        vmc(1),             vImpanc_s89_accc,        vIdedes_l93_accc,
        vIdedes_s93_accc,   vIdeori_l94_accc,        vIdeori_s94_accc,
        vSIGNO(1),          vmontneto(1),            vmac(1),
        vmcc(1),            vnac(1),                 vncc(1),
        vcc_vcac(1),        vcd_vca_vccc(1),         videter_p41_accc,
        vcombol_p48_accc,   vnpa(1),                 vnco(1),
        vcpa(1),            vcco(1),                 vmoneda,
        vfiller_pxxx_accc,  pcod_entadq,                  'm');
  END IF;
  COMMIT;
END;


PROCEDURE f_findadq(ptabla varchar2, vbanco varchar2, vcad1 out varchar2,vcad2 out varchar2) IS
BEGIN

  if ptabla is null then
     if trim(vbanco) = 'BM' then
       vcad1 := '98';
       vcad2 := '05';
     end if;
     if trim(vbanco) = 'BP' then
       vcad1 := '99';
       vcad2 := '08';
     end if;
  else

      if trim(ptabla) in ('clr_mx8000',
                    'clr_mx8999',
                    'clr_mx9799',
                    'clr_mx9899',
                    'clr_mx9999',
                    'clr_mx0108',
                    'clr_ex0108',
                    'clr_ex0105',
                    'clr_ex8001',
                    'clr_ex8010',
                    'clr_ex8020',
                    'clr_ex9001',
                    'clr_ex9010',
                    'clr_copae0108',
                    'clr_copae0105',
                    'clr_copae8001',
                    'clr_copae8010',
                    'clr_copae8020',
                    'clr_copae9001',
                    'clr_copae9010',
                    'clr_copam0108',
                    'clr_icopam8000',
                    'clr_iex0108',
                    'clr_iex8001',
                    'clr_iex8010',
                    'clr_iex8020',
                    'clr_iex0108') and trim(vbanco) = 'BP' then
           vcad1 := '99';
           vcad2 := '08';
        end if;

        if ltrim(rtrim(ptabla)) in('clr_mx8000',
                    'clr_mx8998',
                    'clr_mx9798',
                    'clr_mx9898',
                    'clr_mx9998',
                    'clr_mx0105',
                    'clr_ex0105',
                    'clr_ex0108',
                    'clr_ex8001',
                    'clr_ex8010',
                    'clr_ex8020',
                    'clr_ex9001',
                    'clr_ex9010',
                    'clr_copae0105',
                    'clr_copae0108',
                    'clr_copae8001',
                    'clr_copae8010',
                    'clr_copae8020',
                    'clr_copae9001',
                    'clr_copae9010',
                    'clr_copam0105',
                    'clr_icopam8000',
                    'clr_iex0105',
                    'clr_iex8001',
                    'clr_iex8010',
                    'clr_iex8020'
                    ) and ltrim(rtrim(vbanco)) = 'BM' then
           vcad1 := '98';
           vcad2 := '05';
        end if;

     end if;
END;


PROCEDURE get_compra_m(tabla varchar2, phra_proceso CHAR, videmen_p00_actc number,   vcodpro_p03_actc varchar2,
                       vcodpro_p03_actc_01 varchar2,  vlote  varchar2,           vmoneda  varchar2,
                       vfecha varchar2,               pcod_hrcierre varchar2,       padqui  varchar2,
                       pP04OUT OUT vP04OUT_ARRAY,
                       pP46OUT OUT vP46OUT_ARRAY,
                       pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(tabla,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
  --query_str := 'select nvl(sum(imptra_p04_actc),0) n1, nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0) n2, nvl(count(*),0) n3
    query_str := 'select nvl(sum(imptra_p04_actc),0) n1, nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0) n2,
                         nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                      + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0) n3
                   from '||tabla||'
                  where idemen_p00_actc             =  : 1
                    and (to_number(codpro_p03_actc) >= : 2
                    and to_number(codpro_p03_actc)  <  : 3)
                    and inlote_p29_actc             =  : 4
                    and decode(codfun_p24_actc,205,nvl(moncon_p50_actc,montra_p49_actc),montra_p49_actc)  =  : 5
                    and sesion_p28_actc             =  : 6
                    and cod_hrcierre                =  : 7
                    and substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                    AND tipmov_p48_actc               =  : 8
                    AND id_clrload                    =  : 9';

     -- CREDITO
     EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'c',vID_CLRLOAD;
     -- DEBITO
     EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'d',vID_CLRLOAD;
     -- ALIMENTACION
     EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'a',vID_CLRLOAD;

  end if;

  if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
      pP04OUT(1):= 0;
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
      pP04OUT(2):= 0;
      pP46OUT(2):= 0;
      pNUMREG(2):= 0;
  end if;

  if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
      pP04OUT(3):= 0;
      pP46OUT(3):= 0;
      pNUMREG(3):= 0;
  end if;

END;


PROCEDURE get_compra_e(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2,  vcodpro_p03_actc_01 VARCHAR2,
                        vlote VARCHAR2, vmoneda VARCHAR2,        vemisor VARCHAR2,           vfecha VARCHAR2,
                        pcod_hrcierre VARCHAR2,  padqui varchar2,
                        pP04OUT OUT vP04OUT_ARRAY,
                        pP46OUT OUT vP46OUT_ARRAY,
                        pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
 --query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0), nvl(count(*),0)
   query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0),
                       nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                    + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0)
                  FROM '||tabla||'
                 WHERE idemen_p00_actc             =  : 1
                   AND (TO_NUMBER(codpro_p03_actc) >= : 2
                   AND TO_NUMBER(codpro_p03_actc)  <  : 3)
                   AND inlote_p29_actc             =  : 4
                   and decode(codfun_p24_actc,205,nvl(moncon_p50_actc,montra_p49_actc),montra_p49_actc)  =  : 5
                   AND SUBSTR(nummen_p71_actc,1,4) =  : 6
                   AND sesion_p28_actc             =  : 7
                   AND cod_hrcierre                =  : 8
                   AND substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND tipmov_p48_actc               =  : 9
                   AND id_clrload                    =  : 10';

      -- CREDITO
      EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote, vmoneda, vemisor, vfecha, pcod_hrcierre,'c',vID_CLRLOAD;
      -- DEBITO
      EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote, vmoneda, vemisor, vfecha, pcod_hrcierre,'d',vID_CLRLOAD;
      -- ALIMENTACION
      EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote, vmoneda, vemisor, vfecha, pcod_hrcierre,'a',vID_CLRLOAD;

  end if;

  if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
      pP04OUT(1):= 0;
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
      pP04OUT(2):= 0;
      pP46OUT(2):= 0;
      pNUMREG(2):= 0;
  end if;

  if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
      pP04OUT(3):= 0;
      pP46OUT(3):= 0;
      pNUMREG(3):= 0;
  end if;

END;


PROCEDURE get_devolucion_m(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2 ,   vcodpro_p03_actc_01 VARCHAR2,
                           vlote VARCHAR2,         vmoneda VARCHAR2,        vfecha VARCHAR2,              pcod_hrcierre VARCHAR2,
                           padqui varchar2,
                           pP04OUT OUT vP04OUT_ARRAY,
                           pP46OUT OUT vP46OUT_ARRAY,
                           pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(tabla,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
  --query_str := 'select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0), nvl(count(*),0)
    query_str := 'select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0),
                         nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                      + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0)
                    FROM '||tabla||'
                   WHERE idemen_p00_actc             =  : 1
                     AND (TO_NUMBER(codpro_p03_actc) >= : 2
                     AND TO_NUMBER(codpro_p03_actc)  <  : 3)
                     AND inlote_p29_actc             =  : 4
                     AND montra_p49_actc             =  : 5
                     AND sesion_p28_actc             =  : 6
                     AND cod_hrcierre                =  : 7
                     and substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                     AND tipmov_p48_actc             =  : 8
                     AND id_clrload                  =  : 9';

   -- CREDITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'c',vID_CLRLOAD;
   -- DEBITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'d',vID_CLRLOAD;
   -- ALIMENTACION
    EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'a',vID_CLRLOAD;

   end  if;

  if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
      pP04OUT(1):= 0;
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
      pP04OUT(2):= 0;
      pP46OUT(2):= 0;
      pNUMREG(2):= 0;
  end if;

  if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
      pP04OUT(3):= 0;
      pP46OUT(3):= 0;
      pNUMREG(3):= 0;
  end if;

END;


PROCEDURE get_devolucion_e(tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER,   vcodpro_p03_actc VARCHAR2,  vcodpro_p03_actc_01 VARCHAR2,
                            vlote VARCHAR2,      vmoneda VARCHAR2,          vemisor VARCHAR2,           vfecha VARCHAR2,
                            pcod_hrcierre VARCHAR2, padqui varchar2,
                            pP04OUT OUT vP04OUT_ARRAY,
                            pP46OUT OUT vP46OUT_ARRAY,
                            pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
   --query_str := 'select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0), nvl(count(*),0)
     query_str := 'select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0),
                          nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                       + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0)
                     FROM '||tabla||'
                    WHERE idemen_p00_actc              =  : 1
                      AND (TO_NUMBER(codpro_p03_actc)  >= : 2
                      AND TO_NUMBER(codpro_p03_actc)   <  : 3)
                      AND inlote_p29_actc              =  : 4
                      AND montra_p49_actc              =  : 5
                      AND SUBSTR(nummen_p71_actc,1,4) =  : 6
                      AND sesion_p28_actc             =  : 7
                      AND cod_hrcierre                =  : 8
                      AND substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                      AND tipmov_p48_actc               =  : 9
                      AND id_clrload                    =  : 10';

   -- CREDITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'c',vID_CLRLOAD;
   -- DEBITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'d',vID_CLRLOAD;
   -- ALIMENTACION
    EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'a',vID_CLRLOAD;

   end if;

  if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
      pP04OUT(1):= 0;
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
      pP04OUT(2):= 0;
      pP46OUT(2):= 0;
      pNUMREG(2):= 0;
  end if;

  if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
      pP04OUT(3):= 0;
      pP46OUT(3):= 0;
      pNUMREG(3):= 0;
  end if;

END;


PROCEDURE get_anulacompra_m( tabla VARCHAR2, phra_proceso CHAR, videmen_p00_actc NUMBER, vcodpro_p03_actc VARCHAR2,   vcodpro_p03_actc_01 VARCHAR2,
                             vlote VARCHAR2, vmoneda VARCHAR2,        vfecha VARCHAR2,             pcod_hrcierre VARCHAR2,
                             padqui varchar2,
                             pP04OUT OUT vP04OUT_ARRAY,
                             pP46OUT OUT vP46OUT_ARRAY,
                             pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(tabla,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
 --query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0), nvl(count(*),0)
   query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0),
                       nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                    + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0)
                  FROM '||tabla||'
                 WHERE idemen_p00_actc             =  : 1
                   AND (TO_NUMBER(codpro_p03_actc) >= : 2
                   AND TO_NUMBER(codpro_p03_actc)  <  : 3)
                   AND inlote_p29_actc             =  : 4
                   AND refadq_p31_actc            IS NOT NULL
                   AND montra_p49_actc             =  : 5
                   AND sesion_p28_actc             =  : 6
                   AND cod_hrcierre                =  : 7
                   and substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND tipmov_p48_actc               =  : 8
                   AND id_clrload                    =  : 9';

   -- CREDITO
   EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'c',vID_CLRLOAD;
   -- DEBITO
   EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'d',vID_CLRLOAD;
   -- ALIMENTACION
   EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vfecha,pcod_hrcierre,'a',vID_CLRLOAD;

  end if;

  if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
      pP04OUT(1):= 0;
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
      pP04OUT(2):= 0;
      pP46OUT(2):= 0;
      pNUMREG(2):= 0;
  end if;

  if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
      pP04OUT(3):= 0;
      pP46OUT(3):= 0;
      pNUMREG(3):= 0;
  end if;

END;


PROCEDURE get_anulacompra_e( tabla VARCHAR2,  phra_proceso CHAR, videmen_p00_actc NUMBER,
                             vcodpro_p03_actc VARCHAR2, vcodpro_p03_actc_01 VARCHAR2,
                             vlote VARCHAR2,  vmoneda VARCHAR2,  vemisor VARCHAR2,           vfecha VARCHAR2,
                             pcod_hrcierre VARCHAR2, padqui varchar2,
                             pP04OUT OUT vP04OUT_ARRAY,
                             pP46OUT OUT vP46OUT_ARRAY,
                             pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           VARCHAR2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP04OUT:=vP04OUT_ARRAY(0,0,0);
   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
 --query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0), nvl(count(*),0)
   query_str :='select nvl(sum(imptra_p04_actc),0), nvl(sum(nvl(impcu1_p46_actc, 0) + nvl(impcu2_p46_actc, 0)), 0),
                       nvl(count(*) + sum(pqpclr_hora.f_findprov(indpre_p48_actc, filler_p48_actc, numgui_p48_actc, null))
                                    + sum(pqpclr_hora.f_findcash(mcashb_p48_actc, null)), 0)
                  FROM '||tabla||'
                 WHERE idemen_p00_actc              =  : 1
                   AND (TO_NUMBER(codpro_p03_actc)  >= : 2
                   AND TO_NUMBER(codpro_p03_actc)   <  : 3)
                   AND inlote_p29_actc              =  : 4
                   AND refadq_p31_actc              IS NOT NULL
                   AND montra_p49_actc              =  : 5
                   AND SUBSTR(nummen_p71_actc,1,4) =  : 6
                   AND sesion_p28_actc             =  : 7
                   AND cod_hrcierre                =  : 8
                   AND substr(ideadq_p32_actc,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND tipmov_p48_actc               =  : 9
                   AND id_clrload                    =  : 10';

    -- CREDITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(1), pP46OUT(1), pNUMREG(1) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'c',vID_CLRLOAD;
    -- DEBITO
    EXECUTE IMMEDIATE query_str INTO pP04OUT(2), pP46OUT(2), pNUMREG(2) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'d',vID_CLRLOAD;
    -- ALIMENTACION
    EXECUTE IMMEDIATE query_str INTO pP04OUT(3), pP46OUT(3), pNUMREG(3) USING videmen_p00_actc, vcodpro_p03_actc, vcodpro_p03_actc_01, vlote,vmoneda,vemisor,vfecha,pcod_hrcierre,'a',vID_CLRLOAD;

    end if;

    if pP04OUT(1) is null or pP46OUT(1) is null or pNUMREG(1) is null  then
        pP04OUT(1):= 0;
        pP46OUT(1):= 0;
        pNUMREG(1):= 0;
    end if;

    if pP04OUT(2) is null or pP46OUT(2) is null or pNUMREG(2) is null  then
        pP04OUT(2):= 0;
        pP46OUT(2):= 0;
        pNUMREG(2):= 0;
    end if;

    if pP04OUT(3) is null or pP46OUT(3) is null or pNUMREG(3) is null  then
        pP04OUT(3):= 0;
        pP46OUT(3):= 0;
        pNUMREG(3):= 0;
    end if;

END;


PROCEDURE get_copa_m(tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                     vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                     vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                     pP46OUT OUT vP46OUT_ARRAY,
                     pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           varchar2(1200):='';
   vtiptra             varchar2(2);
   vemisor             varchar2(4);
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if vlote = '314' OR vlote = '316' THEN
      if vlote = '314' then
         vtiptra := '01';
         vemisor := '8001';
      elsif vlote = '316' then
         vtiptra := '06';
         vemisor := '8010';
      end if;
      --20080704 gregando el lote 312
   elsif vlote = '312' then

      if vregdat_p72_accp = '9999' then
         vtiptra := '01';
         vemisor := '8001';
      elsif vregdat_p72_accp = '9998' then
         vtiptra := '01';
         vemisor   := '8001';
      elsif vregdat_p72_accp = '9899' then
         vtiptra := '06';
         vemisor   := '8010';
      elsif vregdat_p72_accp = '9898' then
         vtiptra := '06';
         vemisor   := '8010';
      end if;

   elsif vlote = '311' then

      if vregdat_p72_accp = '9999' then
         vtiptra := '01';
         vemisor := '0105';  -- cobros pagos del otro banco
      elsif vregdat_p72_accp = '9998' then
         vtiptra := '01';
         vemisor   := '0108';
      elsif vregdat_p72_accp = '9899' then
         vtiptra := '06';
         vemisor   := '0105';
      elsif vregdat_p72_accp = '9898' then
         vtiptra := '06';
         vemisor   := '0108';
      end if;

   end if;

pqmonproc.inslog(vIDProc, 'M', 'vlote '||vlote|| ' vregdat_p72_accp '||vregdat_p72_accp||' vtiptra '||vtiptra||' vemisor '||vemisor);

   if Length(trim(vstr1)) > 0 then
      if vlote = '311' then
         query_str :='select nvl(sum(c.impcu1_p46_accp),0), nvl(count(*),0)
                  FROM '||tabla||' c, bines_nacionales b
                 WHERE c.idemen_p00_accp    = :1
                   AND ((c.codfun_p24_accp  = :2    AND c.sigcu1_p46_accp = : 3)
                    OR (c.codfun_p24_accp   = :4     AND c.sigcu1_p46_accp = : 5 ))
                   AND c.inlote_p29_accp    = :6
                   AND SUBSTR(c.regdat_p72_accp,21,4) = : 7
                   AND c.sesion_p28_accp              = : 8
                   AND c.cod_hrcierre                 = : 9
                   and substr(c.numtar_p02_accp,1,6) = b.bin
                   and substr(b.p48tiptra,1,2) = : 10
                   and substr(c.ideadq_p32_accp,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND id_clrload                    =  : 11';
          execute immediate query_str into pP46OUT(1), pNUMREG(1) using videmen_p00_accp, vcodfun_p24_accp , vsigcu1_p46_accp, vcodfun_p24_accp_01, vsigcu1_p46_accp_01, vlote, vemisor,vfecha,pcod_hrcierre, vtiptra, vID_CLRLOAD;
      else
           query_str :='select nvl(sum(c.impcu1_p46_accp),0), nvl(count(*),0)
                  FROM '||tabla||' c
                 WHERE c.idemen_p00_accp    = :1
                   AND ((c.codfun_p24_accp  = :2    AND c.sigcu1_p46_accp = : 3)
                    OR (c.codfun_p24_accp   = :4     AND c.sigcu1_p46_accp = : 5 ))
                   AND c.inlote_p29_accp    = :6
                   AND SUBSTR(c.regdat_p72_accp,21,4) = : 7
                   AND c.sesion_p28_accp              = : 8
                   AND c.cod_hrcierre                 = : 9
                   and substr(c.ideadq_p32_accp,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND id_clrload                    =  : 10';
           execute immediate query_str into pP46OUT(1), pNUMREG(1) using videmen_p00_accp, vcodfun_p24_accp , vsigcu1_p46_accp, vcodfun_p24_accp_01, vsigcu1_p46_accp_01, vlote, vemisor,vfecha,pcod_hrcierre, vID_CLRLOAD;
      end if;

  end if;

  if pP46OUT(1) is null or pNUMREG(1) is null  then
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  pP46OUT(2):= 0;
  pNUMREG(2):= 0;
  pP46OUT(3):= 0;
  pNUMREG(3):= 0;

END;


PROCEDURE get_cobro_e( tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                       vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                       vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                       pP46OUT OUT vP46OUT_ARRAY,
                       pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           varchar2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

  if Length(trim(vstr1)) > 0 then
  query_str :='select nvl(sum(impcu1_p46_accp),0), nvl(count(*),0)
                  from '||tabla||'
                 where idemen_p00_accp    = :1
                   and ((codfun_p24_accp  = :2    and sigcu1_p46_accp = : 3 )
                    or (codfun_p24_accp   = :4    and sigcu1_p46_accp = : 5 ))
                   and inlote_p29_accp    = :6
                   and substr(regdat_p72_accp,21,4) = : 7
                   and sesion_p28_accp              = : 8
                   and cod_hrcierre                 = : 9
                   and substr(ideadq_p32_accp,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   and id_clrload                    =  : 10';

   -- MISCELANEOS
   execute immediate query_str into pP46OUT(1), pNUMREG(1) using videmen_p00_accp, vcodfun_p24_accp , vsigcu1_p46_accp, vcodfun_p24_accp_01, vsigcu1_p46_accp_01, vlote, vregdat_p72_accp,vfecha,pcod_hrcierre,vID_CLRLOAD;
   end if;

   if pP46OUT(1) is null or pNUMREG(1) is null  then
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
   end if;

   pP46OUT(2):= 0;
   pNUMREG(2):= 0;
   pP46OUT(3):= 0;
   pNUMREG(3):= 0;

END;

PROCEDURE get_pago_e(tabla varchar2, phra_proceso CHAR, videmen_p00_accp number,       vcodfun_p24_accp varchar2 ,    vsigcu1_p46_accp varchar2,
                      vcodfun_p24_accp_01 varchar2,   vsigcu1_p46_accp_01 varchar2,  vlote varchar2,                vregdat_p72_accp varchar2,
                      vfecha varchar2,                pcod_hrcierre varchar2,           padqui varchar2,
                      pP46OUT OUT vP46OUT_ARRAY,
                      pNUMREG OUT vNUMREG_ARRAY)
AS
   query_str           varchar2(1200):='';
   vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
   vFecSesionLoad        date;
BEGIN

   pP46OUT:=vP46OUT_ARRAY(0,0,0);
   pNUMREG:=vNUMREG_ARRAY(0,0,0);

   f_findadq(null,padqui,vstr1,vstr2);

   vFecSesionLoad := PQPLIQ.GetFecSes2Proc(TO_DATE(vfecha,'YYMMDD'),phra_proceso,pcod_hrcierre);

   vID_CLRLOAD := PQPCLRLOAD.getidclrload( vFecSesionLoad, UPPER(SUBSTR(tabla,5)), phra_proceso);

   if Length(trim(vstr1)) > 0 then
   query_str :='select nvl(sum(impcu1_p46_accp),0), nvl(count(*),0)
                  from '||tabla||'
                 where idemen_p00_accp    = :1
                   and ((codfun_p24_accp  = :2    and sigcu1_p46_accp = : 3 )
                    or (codfun_p24_accp   = :4    and sigcu1_p46_accp = : 5 ))
                   and inlote_p29_accp    = :6
                   and substr(regdat_p72_accp,21,4) = : 7
                   and sesion_p28_accp              =  : 8
                   and cod_hrcierre                 =  : 9
                   and substr(ideadq_p32_accp,5,2) in ('||chr(39)||vstr1||chr(39)||','||chr(39)||vstr2||chr(39)||')
                   AND id_clrload                   =  : 10';

  -- MISCELANEOS
  execute immediate query_str into pP46OUT(1), pNUMREG(1) using videmen_p00_accp, vcodfun_p24_accp , vsigcu1_p46_accp, vcodfun_p24_accp_01, vsigcu1_p46_accp_01, vlote, vregdat_p72_accp,vfecha,pcod_hrcierre,vID_CLRLOAD;

  end if;

  if pP46OUT(1) is null or pNUMREG(1) is null  then
      pP46OUT(1):= 0;
      pNUMREG(1):= 0;
  end if;

  pP46OUT(2):= 0;
  pNUMREG(2):= 0;
  pP46OUT(3):= 0;
  pNUMREG(3):= 0;

END;

FUNCTION f_clr_gendatrpt (pEntAdq CHAR, pCodHrCierre CHAR, phra_proceso CHAR, pFecSesion DATE) RETURN CHAR
IS

vCodMoneda      CHAR(3) := '862' ;
vDescMoneda     VARCHAR2(20) := 'Bolivares';

--vIdProc         NUMBER;
vRetC           VARCHAR2(100);
vOraCode        NUMBER;
vOraErr         VARCHAR2(200);
vErrMsg         VARCHAR2(200);
vErrCod         VARCHAR2(2);
vPaso           VARCHAR2(10);
vImpNetoA     NUMBER:=0;  -- Importe Acreedor Por Lote
vImpNetoD     NUMBER:=0;  -- Importe Deudor Por Lote

PROCEDURE InsDatRptRes
IS
BEGIN
    -- Elimina informacion anterior
    pqmonproc.inslog(vIDProc, 'M', 'Eliminando informacion anterior de clr_resumen_hora.');
    DELETE CLR_RESUMEN_HORA
      WHERE fec_sesion  = pFecSesion
       AND cod_hrcierre = pCodHrCierre
       AND hra_proceso  = phra_proceso
       AND cod_entadq   = pEntAdq;

   DELETE clr_resumen_acumulado
      WHERE fec_sesion  = pFecSesion
       AND cod_hrcierre = pCodHrCierre
       AND cod_entadq   = pEntAdq;
    -- Emisor
    pqmonproc.inslog(vIDProc, 'M', 'Procesando informacion de Emisor.');
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
                FROM CLR_LETOTA_HORA
               WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                 AND cod_hrcierre = pCodHrCierre
                 AND hra_proceso = phra_proceso
                 AND cod_entadq = pEntAdq
            ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
    LOOP
        INSERT INTO clr_resumen_hora
            (fec_sesion, cod_hrcierre, hra_proceso, cod_moneda, tipo_lote, cod_entadq,
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
            (pfecsesion, pcodhrcierre, phra_proceso, r.moneda_pxxx_accc, 'E', pentadq,
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
     pqmonproc.inslog(vIDProc, 'M', 'Procesando informacion de Merchant.');
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
                 FROM CLR_LMTOTA_HORA
                WHERE sesion_p28_accc = TO_CHAR(pFecSesion,'YYMMDD')
                  AND cod_hrcierre = pCodHrCierre
                  AND hra_proceso = phra_proceso
                  AND cod_entadq = pEntAdq
             ORDER BY SUBSTR(ideori_s94_accc,3,4), signox_s97_accc, inlote_p29_accc)
     LOOP
        INSERT INTO clr_resumen_hora
            (fec_sesion, cod_hrcierre, hra_proceso, cod_moneda, tipo_lote, cod_entadq,
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
            (pfecsesion, pcodhrcierre, phra_proceso, r.moneda_pxxx_accc, 'M', pentadq,
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

        SELECT NVL(SUM(NVL(IMP_NETO,0)),0) IMP_NETO
        INTO vImpNetoD
        FROM CLR_RESUMEN_HORA
       WHERE FEC_SESION = pFecSesion
         AND COD_HRCIERRE = pCodHrCierre
         AND TIPO_SALDO = 'D'
         AND COD_ENTADQ = pEntAdq;

      -- ACREEDOR
      SELECT NVL(SUM(NVL(IMP_NETO,0)),0) IMP_NETO
        INTO vImpNetoA
        FROM CLR_RESUMEN_HORA
       WHERE FEC_SESION = pFecSesion
         AND COD_HRCIERRE = pCodHrCierre
         AND TIPO_SALDO = 'C'
         AND COD_ENTADQ = pEntAdq;

         insert into clr_resumen_acumulado
         values(pFecSesion,pCodHrCierre,pEntAdq,vImpNetoD,vImpNetoA );
         commit;

END;

BEGIN

    /* ID DE PROCESO
    ****************/
    vIDProc:= pqmonproc.InsMonProc ('PCLRHDAT');

    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre||' | HProc: '||phra_proceso);
    pqmonproc.inslog(vIDProc, 'M', 'Inicia Proceso Genera Informacion para Reportes');

    InsDatRptRes;

    /* FIN DE PROCESO
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'Fin OK Proceso Genera Informacion para Reportes');
    vRetC  := pqmonproc.updmonproc (vIDProc, 'F');
    RETURN '0~';

EXCEPTION
    WHEN others THEN
         ROLLBACK;
         voracode := abs(sqlcode);
         voraerr  := substr(sqlerrm,1,200);
         pqmonproc.InsLog(vIDProc, 'E', vOraErr);
         vretc    := pqmonproc.updmonproc (vIDProc, 'E', '1');
         RETURN 'EERROR de Base de Datos (ORA-'||LPAD(vOraCode,5,'0')||')~';
END;


FUNCTION f_clr_rptcuadre (pEntAdq CHAR, pCodHrCierre CHAR, phra_proceso CHAR, pFecSesion DATE) RETURN CHAR
IS

vDirOUT       VARCHAR2(100);
vFileRPT      VARCHAR2(50);

vImpNetoA     NUMBER:=0;  -- Importe Acreedor Por Lote
vImpNetoD     NUMBER:=0;  -- Importe Deudor Por Lote
vImpAcumA     NUMBER:=0;  -- Importe Acreedor Por Lote Acumulado
vImpAcumD     NUMBER:=0;  -- Importe Deudor Por Lote Acumulado
vTotDeudor    NUMBER:=0;  -- Total Deudor
vTotAcreedor  NUMBER:=0;  -- Total Acreedor

vCodLCSB      ENTIDADES_PRICE.c00lcsb%TYPE;
vDescEntAdq   ENTIDADES_PRICE.c00nredu%TYPE;
vBinADQ       VARCHAR2(6);
vFlgCuadre    CHAR(1):='S';
vDummyTXT     VARCHAR2(80);
vIDFile       UTL_FILE.FILE_TYPE;

--vIdProc       NUMBER;
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
        FROM CLR_RESUMEN_HORA
       WHERE FEC_SESION = pFecSesion
         AND TIPO_LOTE = pTipoLote
         AND COD_HRCIERRE = pCodHrCierre
         AND hra_proceso = phra_proceso
         AND TIPO_SALDO = 'D'
         AND COD_ENTADQ = pEntAdq;

      -- ACREEDOR
      SELECT NVL(SUM(NVL(IMP_NETO,0)),0) IMP_NETO
        INTO vImpNetoA
        FROM CLR_RESUMEN_HORA
       WHERE FEC_SESION = pFecSesion
         AND TIPO_LOTE = pTipoLote
         AND COD_HRCIERRE = pCodHrCierre
         AND hra_proceso = phra_proceso
         AND TIPO_SALDO = 'C'
         AND COD_ENTADQ = pEntAdq;

      -- ACUMULADO
      SELECT NVL(imp_deudor,0) IMP_DEUDOR_ACUM,
             NVL(imp_acreedor,0) IMP_ACREEDOR_ACUM
        INTO vImpAcumD, vImpAcumA
        FROM CLR_RESUMEN_ACUMULADO
       WHERE FEC_SESION = pFecSesion
         AND COD_HRCIERRE = pCodHrCierre
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

    /* ID DE PROCESO
    ****************/
    vIDProc:= pqmonproc.InsMonProc ('PCLRHRPT');

    /* PROCEDIMIENTOS
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'EntAdq: '||pEntAdq||' | FecSes: '||TO_CHAR(pFecSesion,'YYYY-MM-DD')||' | HCierre: '||pCodHrCierre||' | HProc: '||phra_proceso);
    pqmonproc.inslog(vIDProc, 'M', 'Inicia Proceso Genera Reporte de Cuadre');


   vPaso := 'Paso 1';
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
   SELECT 'CLR'||pEntAdq||TO_CHAR(pFecSesion,'YYYYMMDD')||'.'||pCodHrCierre||'.'||phra_proceso||'.RPT'
     INTO vFileRPT
     FROM DUAL;

   SELECT C00NREDU,
          C00LCSB
     INTO vDescEntAdq,
          vCodLCSB
     FROM ENTIDADES_PRICE
    WHERE COD_ENTADQ = pEntAdq
      AND c00ltipo = 0;

   vPaso := 'Paso 3';
   -- Abre el Archivo de Reporte
      vIDFile:=UTL_FILE.FOPEN(vDirOUT,vFileRPT,'W');
   IF NOT UTL_FILE.IS_OPEN(vIDFile) THEN
      vErrMsg:='ERROR | Archivo <'||vFileRPT||'> NO Creado.';
      VErrCod:= '4';
      RAISE eFinError;
   END IF;

   vPaso := 'Paso 3.1';
   pPutTXT(' ');
   pPutTXT('*');
   pPutTXT('SISTEMA DE GESTION DE COMERCIOS','S');
   pPutTXT('REPORTE DE CUADRE DE CLEARING POR HORA','S');
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
   SELECT RPAD(' ',53,' ')||'Hora de Proceso: '||phra_proceso
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
    where sesion_p28_actc = TO_CHAR(pFecSesion,'YYMMDD')
        and cod_hrcierre    = pCodHrCierre
        AND SUBSTR(ideadq_p32_actc,3,4) IN (SELECT C00LCSB
                                              FROM ENTIDADES_PRICE
                                             WHERE COD_ENTADQ = pEntAdq
                                               AND C00LTIPO = 0)
        AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                           WHERE FEC_SESION = PQPLIQ.GetFecSes2Proc(pFecSesion,phra_proceso,pCodHrCierre)
                               AND COD_ARCHIVO = 'MX8000'
                               AND HRA_PROCESO = phra_proceso)
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
      pPutTXT('PROCESO DE CLEARING POR HORA FINALIZADO CORRECTAMENTE','S','--');
   ELSE
         IF (vImpAcumA-vImpAcumD) = 0  THEN
            pPutTXT(' ');
              pPutTXT('-');
              pPutTXT('PROCESO DE CLEARING ACUMULADO HASTA LA HORA ' || phra_proceso || ' FINALIZADO CORRECTAMENTE','S','--');
         ELSE
              -- Descuadre
              vFlgCuadre:='N';
              pPutTXT(' ');
              pPutTXT('#');
              pPutTXT('      ###           DESCUADRE EN PROCESO DE CLEARING POR HORA          ###');
              pPutTXT('      ###                  Diferencia: '||num2char(vTotAcreedor-vTotDeudor,'I')||'                  ###');
              pPutTXT('#');
              pPutTXT(' ');
         END IF;
   END IF;

   vPaso := 'Paso 7';
   pPutTXT('-');
   pPutTXT('Telefonica Servicios Transaccionales'||LPAD(TO_CHAR(SYSDATE,'DD/MM/YYYY')||' '||TO_CHAR(SYSDATE,'HH24:MI:SS'),44,' '));
   pPutTXT(' ');
   UTL_FILE.FCLOSE(vIDFile);

   IF vFlgCuadre = 'N' THEN
      vDummyTXT:=pEntAdq||' | Cierre: '||pCodHrCierre||' | Hora: '||phra_proceso;
      PQMONPROC.InsLog(vIDProc, 'E', 'Descuadre en Proceso de Clearing por Hora '||vDummyTXT);
      vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '1');
      RETURN '1';
   ELSE
      PQMONPROC.InsLog(vIDProc, 'M', 'Termino proceso, archivo: '||vDirOUT||'/'||vFileRPT);
      vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'F');
      RETURN '0';
   END IF;

    /* FIN DE PROCESO
    *****************/
    pqmonproc.inslog(vIDProc, 'M', 'Fin OK Proceso Genera Reporte de Cuadre');
    vRetC  := pqmonproc.updmonproc (vIDProc, 'F');
    RETURN '0~';

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
         vRetC :=PQMONPROC.UpdMonProc (vIDProc, 'E', '99');
         RETURN vErrCod||vErrMsg;
END;


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- FUNCTION F_INTERFAZ_PLIQ
-- Interfaz para el Proceso de Liquidacion
-- Version Original: Walter Cisneros
-- SSM 20070709: Optimizacion para el Reproceso y Log de Proceso
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

function f_interfaz_pliq (pFecSesion DATE, pCodHRCierre CHAR,phra_proceso CHAR, pOpcRepro CHAR:=NULL) return char
is

  --  vIdProc               number;
    vRetC                 varchar2(100);
    vCont                 number(10);
    vFlgRev               boolean:=FALSE;
    vOraCode              number;
    vID_CLRLOAD         CTL_CLRLOAD.ID_CLRLOAD%TYPE;
begin

  vIDProc := pqmonproc.InsMonProc ('PCLRINTPLIQHR');
  -- vID_CLRLOAD := PQPCLRLOAD.getidclrload( TO_DATE(vfecha,'YYMMDD'), UPPER(SUBSTR(tabla,5)), phra_proceso);
  if pOpcRepro = 'S' then
     vFlgRev:=TRUE;
  elsif pOpcRepro is NULL then
        select count(*)
          into vCont
          from tmp_pclm_actc
         where p28sesion = to_char(pFecSesion,'RRMMDD')
           and cod_hrcierre = pCodHRCierre
           AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                       WHERE FEC_SESION = pFecSesion
                       AND COD_ARCHIVO like 'MX%'
                       AND HRA_PROCESO = phra_proceso);
        if vCont > 0 then
           vFlgRev:=TRUE;
        end if;
  end if;

  -- Reversion de la Carga
  if vFlgRev then
     pqmonproc.inslog(vIDProc, 'M', 'REPROCESO: Eliminando Informacion Anterior...');
     -- Elimina Informacion de TMP_PCLM_ACTC
     vCont:=0;
     for r in (select rowid IDreg
                 from tmp_pclm_actc
                where p28sesion = to_char(pFecSesion,'RRMMDD')
                  and cod_hrcierre = pCodHRCierre
                  AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                       WHERE FEC_SESION = pFecSesion
                       AND COD_ARCHIVO like 'MX%'
                       AND HRA_PROCESO = phra_proceso)) loop
         delete tmp_pclm_actc
          where rowid = r.IDreg;
         vCont:=vCont+1;
         if mod(vCont,1000) = 0 then
            commit;
         end if;
     end loop;
     commit;
     -- Elimina Informacion de TMP_PCLM_ACCP
     vCont:=0;
     for r in (select rowid IDreg
                 from tmp_pclm_accp
                where p28sesion    = to_char(pFecSesion,'RRMMDD')
                  and cod_hrcierre = pCodHRCierre
                  AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                       WHERE FEC_SESION = pFecSesion
                       AND COD_ARCHIVO like 'MX%'
                       AND HRA_PROCESO = phra_proceso)) loop

         delete tmp_pclm_accp
          where rowid = r.IDreg;
         vCont:=vCont+1;

         if mod(vCont,1000) = 0 then
            commit;
         end if;
     end loop;
     commit;
     -- Elimina Informacion de TMP_PCLM_ACMA
     vCont:=0;
     for r in (select rowid IDreg
                 from tmp_pclm_acma
                where p28sesion = to_char(pFecSesion,'RRMMDD')
                  and cod_hrcierre = pCodHRCierre
                  AND id_clrload IN( SELECT id_clrload  FROM CTL_CLRLOAD
                       WHERE FEC_SESION = pFecSesion
                       AND COD_ARCHIVO like 'MX%'
                       AND HRA_PROCESO = phra_proceso)) loop
         delete tmp_pclm_acma
          where rowid = r.IDreg;
         vCont:=vCont+1;
         if mod(vCont,1000) = 0 then
            commit;
         end if;
     end loop;
     commit;
  end if;


  -- Cursor MX0000
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX0000...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx0000',5)), phra_proceso);
    pqmonproc.inslog(vIDProc, 'M', 'vID_CLRLOAD ' || vID_CLRLOAD);
  for r in (select *
              from clr_mx0000
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop
                -- JPC 20071030: se consideran todas las transacciones
                -- and idemen_p00_actc <> 1444

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;

  -- Cursor MX8000
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX8000...');
  vCont:=0;
    vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx8000',5)), phra_proceso);
  for r in (select *
              from clr_mx8000
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;





  -- Cursor MX8998
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX8998...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx8998',5)), phra_proceso);
  for r in (select *
              from clr_mx8998
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;



  -- Cursor MX8999
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX8999...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx8999',5)), phra_proceso);
  for r in (select *
              from clr_mx8999
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;




  -- Cursor MX9798
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX9798...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx9798',5)), phra_proceso);
  for r in (select *
              from clr_mx9798
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,  r.numtar_p02_actc,  r.codpro_p03_actc,
              r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,  r.concon_p09_actc,  r.concli_p10_actc,
              r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,  r.feccon_p16_actc,  r.feccap_p17_actc,
              r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,  r.codfun_p24_actc,  r.codraz_p25_actc,
              r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,  r.impori_p30_actc,  r.oricon_p30_actc,
              r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,  r.ideadq_p32_actc,  r.idepre_l33_actc,
              r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,  r.datref_p37_actc,  r.numaut_p38_actc,
              r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,  r.ideest_p42_actc,  r.nomest_p43_actc,
              r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,  r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,
              r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,  r.codr14_p48_actc,  r.ctaabo_p48_actc,
              r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,  r.lonr18_p48_actc,  r.codr18_p48_actc,
              r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,  r.limcon_p48_actc,  r.lonr50_p48_actc,
              r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,
              r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,  r.oriide_p56_actc,  r.oritra_p56_actc,
              r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,  r.ideaut_l58_actc,  r.ideaut_p58_actc,
              r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,  r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;


  -- Cursor MX9898
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX9898...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx9898',5)), phra_proceso);
  for r in (select *
              from clr_mx9898
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;


  -- Cursor MX9899
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX9899...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx9899',5)), phra_proceso);
  for r in (select *
              from clr_mx9899
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;


  -- Cursor MX9998
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX9998...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload(pFecSesion, UPPER(SUBSTR('clr_mx9998',5)), phra_proceso);
  for r in (select *
              from clr_mx9998
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;


  -- Cursor MX9999
  pqmonproc.inslog(vIDProc, 'M', 'Procesando MX9999...');
  vCont:=0;
  vID_CLRLOAD := PQPCLRLOAD.getidclrload( pFecSesion, UPPER(SUBSTR('clr_mx9999',5)), phra_proceso);
  for r in (select *
              from clr_mx9999
             where sesion_p28_actc = to_char(pFecSesion,'yymmdd')
               and cod_hrcierre    = pCodHRCierre
               and id_clrload      = vID_CLRLOAD) loop

      insert into tmp_pclm_actc
             (id_clrload,       id_registro,      cod_hrcierre,     p00idmsg,         p02numtar_l,
              p02numtar,        p03codpro,        p04imptra,        p05impcon,        p06imptit,
              p09concon,        p10concli,        p11idetra,        p12timloc,        p14feccad,
              p16feccon,        p17feccap,        p18lacti,         p19codpai,        p22punser,
              p24codfun,        p25codraz,        p26lacti,         p28sesion,        p29inlote,
              p30impori,        p30oricon,        p31refadq_l,      p31refadq,        p32idadq_l,
              p32idadq,         p33idpre_l,       p33idpre,         p35pista2_l,      p35pista2,
              p37datref,        p38numaut,        p39codacr,        p40codser,        p41cseri,
              p42ideest,        p43nomest,        p43locest,        p43paiest,        p45pista1_l,
              p45pista1,        p46tcuot01,       p46tcuot02,       p46tcuot03,       p46tcuot04,
              p48londat,        p4813lonr,        p4813codr,        p48tiptra,        p4814lonr,
              p4814codr,        p48cuenta,        p4816lonr,        p4816codr,        p48fecest,
              p4818lonr,        p4818codr,        p4818listne,      p4819lonr,        p4819codr,
              p4819limcon,      p4850lonr,        p4850codr,        p4850vtapla,      p4899filler,
              p48tipmov,        p49montra,        p50moncon,        p51montit,        p56datori_l,
              p56do_idmsg,      p56do_idetra,     p56do_timloc,     p56do_idadq_l,    p56do_idadq,
              p58idaut_l,       p58idaut,         P62MELECT,        p71nummen,        p95numemi_l,
              p95numemi)
      values
             (r.id_clrload,       r.id_registro,      r.cod_hrcierre,     r.idemen_p00_actc,  r.lontar_l02_actc,
              r.numtar_p02_actc,  r.codpro_p03_actc,  r.imptra_p04_actc,  r.impcon_p05_actc,  r.imptit_p06_actc,
              r.concon_p09_actc,  r.concli_p10_actc,  r.idetra_p11_actc,  r.timloc_p12_actc,  r.feccad_p14_actc,
              r.feccon_p16_actc,  r.feccap_p17_actc,  r.codact_p18_actc,  r.codpai_p19_actc,  r.punser_p22_actc,
              r.codfun_p24_actc,  r.codraz_p25_actc,  r.codact_p26_actc,  r.sesion_p28_actc,  r.inlote_p29_actc,
              r.impori_p30_actc,  r.oricon_p30_actc,  r.lonref_l31_actc,  r.refadq_p31_actc,  r.ideadq_l32_actc,
              r.ideadq_p32_actc,  r.idepre_l33_actc,  r.idepre_p33_actc,  r.lonpis_l35_actc,  r.pista2_p35_actc,
              r.datref_p37_actc,  r.numaut_p38_actc,  r.codacr_p39_actc,  r.codser_p40_actc,  r.ideter_p41_actc,
              r.ideest_p42_actc,  r.nomest_p43_actc,  r.locest_p43_actc,  r.paiest_p43_actc,  r.lonpi1_p45_actc,
              r.pista1_p45_actc,
              lpad(nvl(r.tipcu1_p46_actc,0),2,0)|| lpad(nvl(r.sigcu1_p46_actc,0),1,0)|| lpad(nvl(r.impcu1_p46_actc,0),8,0),
              lpad(nvl(r.tipcu2_p46_actc,0),2,0)|| lpad(nvl(r.sigcu2_p46_actc,0),1,0)|| lpad(nvl(r.impcu2_p46_actc,0),8,0),
              lpad(nvl(r.tipcu3_p46_actc,0),2,0)|| lpad(nvl(r.sigcu3_p46_actc,0),1,0)|| lpad(nvl(r.impcu3_p46_actc,0),8,0),
              lpad(nvl(r.tipcu4_p46_actc,0),2,0)|| lpad(nvl(r.sigcu4_p46_actc,0),1,0)|| lpad(nvl(r.impcu4_p46_actc,0),8,0),
              r.londat_p48_actc,  r.lonr13_p48_actc,  r.codr13_p48_actc,  r.tiptra_p48_actc,  r.lonr14_p48_actc,
              r.codr14_p48_actc,  r.ctaabo_p48_actc,  r.lonr16_p48_actc,  r.codr16_p48_actc,  r.fecest_p48_actc,
              r.lonr18_p48_actc,  r.codr18_p48_actc,  r.listne_p48_actc,  r.lonr19_p48_actc,  r.codr19_p48_actc,
              r.limcon_p48_actc,  r.lonr50_p48_actc,  r.codr50_p48_actc,  r.vtapla_p48_actc,
              LPAD(r.vtapla_p48_actc,2,'0')||RPAD(SUBSTR(r.filler_p48_actc,3),11,' ')||LPAD(r.mcashb_p48_actc,10,'0')||LPAD(r.indpre_p48_actc,2,' ')||LPAD(NVL(SUBSTR(r.numgui_p48_actc,2,3),' '),3,' '),
              r.tipmov_p48_actc,  r.montra_p49_actc,  r.moncon_p50_actc,  r.montit_p51_actc,  r.oridat_l56_actc,
              r.oriide_p56_actc,  r.oritra_p56_actc,  r.oritim_p56_actc,  r.oriadq_l56_actc,  r.oriadq_p56_actc,
              r.ideaut_l58_actc,  r.ideaut_p58_actc,  r.FILLER_P62_ACTC,  r.nummen_p71_actc,  r.numemi_l95_actc,
              r.numemi_p95_actc);

      vCont :=  vCont + 1;
      if MOD(vCont,10000) = 0 then
         commit;
      end if;
  end loop;
  commit;


  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  COBROS PAGOS  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  -- JPC 20080103: NO SE CARGAN LOS COBROS PAGOS

  -- -- Cursor COPAM0105
  -- pqmonproc.inslog(vIDProc, 'M', 'Procesando COPAM0105...');
  -- vCont:=0;
  -- for r in (select *
  --             from clr_copam0105
  --            where sesion_p28_accp = to_char(pFecSesion,'yymmdd')
  --              and cod_hrcierre    = pCodHRCierre) loop
  --
  --     insert into tmp_pclm_accp
  --            (id_clrload,           id_registro,         cod_hrcierre,        p00idmsg,              p02numtar_l,
  --             p02numtar,            p11idetra,           p12timloc,           p24codfun,             p25codraz,
  --             p28sesion,            p29inlote,           p32idadq_l,          p32idadq,              p33idpre_l,
  --             p33idpre,              p39codacr,          p46tcuot01,          p46tcuot02,            p46tcuot03,
  --             p46tcuot04,            p56datori_l,        p56do_idmsg,         p56do_idetra,          p56do_timloc,
  --             p56do_idadq_l,         p56do_idadq,        p71nummen,           p72regdat_l,           p72regdat,
  --             p104texto_l,           p104texto,          filler)
  --     values
  --            (r.id_clrload,          r.id_registro,      r.cod_hrcierre,      r.idemen_p00_accp,     r.lontar_l02_accp,
  --             r.numtar_p02_accp,     r.idetra_p11_accp,  r.timloc_p12_accp,   r.codfun_p24_accp,     r.codraz_p25_accp,
  --             r.sesion_p28_accp,     r.inlote_p29_accp,  r.ideadq_l32_accp,   r.ideadq_p32_accp,     r.idepre_l33_accp,
  --             r.idepre_p33_accp,     r.codacr_p39_accp,
  --             lpad(nvl(r.tipcu1_p46_accp,0),2,0)|| lpad(nvl(r.sigcu1_p46_accp,0),1,0)|| lpad(nvl(r.impcu1_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu2_p46_accp,0),2,0)|| lpad(nvl(r.sigcu2_p46_accp,0),1,0)|| lpad(nvl(r.impcu2_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu3_p46_accp,0),2,0)|| lpad(nvl(r.sigcu3_p46_accp,0),1,0)|| lpad(nvl(r.impcu3_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu4_p46_accp,0),2,0)|| lpad(nvl(r.sigcu4_p46_accp,0),1,0)|| lpad(nvl(r.impcu4_p46_accp,0),8,0),
  --             r.oridat_l56_accp,  r.oriide_p56_accp,   r.oritra_p56_accp,     r.oritim_p56_accp,
  --             r.oriadq_l56_accp,     r.oriadq_p56_accp,  r.nummen_p71_accp,   r.regdat_l72_accp,     r.regdat_p72_accp,
  --             r.texto_l104_accp,     r.texto_p104_accp,  r.filler_pxx_accp);
  --
  --     vCont :=  vCont + 1;
  --     if MOD(vCont,10000) = 0 then
  --        commit;
  --     end if;
  -- end loop;
  -- commit;
  --
  --
  -- -- Cursor COPAM0108
  -- pqmonproc.inslog(vidproc, 'M', 'Procesando COPAM0108...');
  -- vCont:=0;
  -- for r in (select *
  --             from clr_copam0108
  --            where sesion_p28_accp = to_char(pFecSesion,'yymmdd')
  --              and cod_hrcierre    = pCodHRCierre) loop
  --
  --     insert into tmp_pclm_accp
  --            (id_clrload,            id_registro,        cod_hrcierre,        p00idmsg,              p02numtar_l,
  --             p02numtar,             p11idetra,          p12timloc,           p24codfun,             p25codraz,
  --             p28sesion,             p29inlote,          p32idadq_l,          p32idadq,              p33idpre_l,
  --             p33idpre,              p39codacr,          p46tcuot01,          p46tcuot02,            p46tcuot03,
  --             p46tcuot04,            p56datori_l,        p56do_idmsg,         p56do_idetra,          p56do_timloc,
  --             p56do_idadq_l,         p56do_idadq,        p71nummen,           p72regdat_l,           p72regdat,
  --             p104texto_l,           p104texto,          filler)
  --     values
  --            (r.id_clrload,       r.id_registro,       r.cod_hrcierre,     r.idemen_p00_accp,   r.lontar_l02_accp,
  --             r.numtar_p02_accp,  r.idetra_p11_accp,   r.timloc_p12_accp,  r.codfun_p24_accp,   r.codraz_p25_accp,
  --             r.sesion_p28_accp,  r.inlote_p29_accp,   r.ideadq_l32_accp,  r.ideadq_p32_accp,   r.idepre_l33_accp,
  --             r.idepre_p33_accp,  r.codacr_p39_accp,
  --             lpad(nvl(r.tipcu1_p46_accp,0),2,0)|| lpad(nvl(r.sigcu1_p46_accp,0),1,0)|| lpad(nvl(r.impcu1_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu2_p46_accp,0),2,0)|| lpad(nvl(r.sigcu2_p46_accp,0),1,0)|| lpad(nvl(r.impcu2_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu3_p46_accp,0),2,0)|| lpad(nvl(r.sigcu3_p46_accp,0),1,0)|| lpad(nvl(r.impcu3_p46_accp,0),8,0),
  --             lpad(nvl(r.tipcu4_p46_accp,0),2,0)|| lpad(nvl(r.sigcu4_p46_accp,0),1,0)|| lpad(nvl(r.impcu4_p46_accp,0),8,0),
  --             r.oridat_l56_accp,   r.oriide_p56_accp,  r.oritra_p56_accp,   r.oritim_p56_accp,
  --             r.oriadq_l56_accp,  r.oriadq_p56_accp,   r.nummen_p71_accp,  r.regdat_l72_accp,   r.regdat_p72_accp,
  --             r.texto_l104_accp,  r.texto_p104_accp,   r.filler_pxx_accp);
  --
  --     vCont :=  vCont + 1;
  --     if MOD(vCont,10000) = 0 then
  --        commit;
  --     end if;
  -- end loop;
  -- commit;

  -- Fin OK
  pqmonproc.inslog(vIDProc, 'M', 'FIN OK | Interfaz PLIQ HORA');
  vretc :=pqmonproc.updmonproc (vIDProc, 'F');
  return '0';

exception
  when others then
       voracode :=ABS(SQLCODE);
       pqmonproc.inslog(vIDProc, 'E', 'Error de Base de Datos (ORA-'||LPAD(voracode,5,'0')||').');
       vretc    :=pqmonproc.updmonproc (vIDProc, 'E', '99');
       return 'E|Error de Base de Datos (ORA-'||LPAD(voracode,5,'0')||').';
end;

FUNCTION f_findprov(pINDPRE_P48_ACTC VARCHAR2, pFILLER_P48_ACTC VARCHAR2, pNUMGUI_P48_ACTC VARCHAR2, pP4899FILLER VARCHAR2) RETURN NUMBER
IS
    vNUMPROV    PLS_INTEGER:=0;
    vINDPROV    VARCHAR2(2):=NULL;
    vTIPPROV    VARCHAR2(1):=NULL;
    vPORPROV    NUMBER(3,2):=0;
BEGIN
    -- IDENTIFICA SI LA TRX PROVIMILLA SE DESDOBLA Y RETORNA LA CANTIDAD DE TRX CREADOS
    -- PARA CONTABILIZARLOS EN EL CLEARING E INTERFAZ DE LIQUIDACION
    IF pP4899FILLER IS NULL THEN
       vINDPROV:=pINDPRE_P48_ACTC;
       vTIPPROV:=SUBSTR(pFILLER_P48_ACTC,11,1);
       vPORPROV:=NVL(TO_NUMBER(TRIM(SUBSTR(pNUMGUI_P48_ACTC,2,3)))/100,0);
    ELSE
       vINDPROV:=SUBSTR(pP4899FILLER,24,2);
       vTIPPROV:=SUBSTR(pP4899FILLER,11,1);
       vPORPROV:=NVL(TO_NUMBER(TRIM(SUBSTR(pP4899FILLER,26,3)))/100,0);
    END IF;

    IF vINDPROV = '04' THEN -- INDICADOR DE PROVIMILLA
       IF vTIPPROV <> '2' AND vPORPROV < 1 THEN -- TIPO DE PAGO Y PORCENTAJE
          vNUMPROV:=1;
       END IF;
    END IF;
    RETURN vNUMPROV;
END;

FUNCTION f_findcash(pMCASHB_P48_ACTC VARCHAR2, pP4899FILLER VARCHAR2) RETURN NUMBER
IS
    vNUMCASH    PLS_INTEGER:=0;
    vIMPCASH    NUMBER(10):=0;
BEGIN
    -- IDENTIFICA SI LA TRX CASHBACK SE DESDOBLA Y RETORNA LA CANTIDAD DE TRX CREADOS
    -- PARA CONTABILIZARLOS EN EL CLEARING E INTERFAZ DE LIQUIDACION
    IF pP4899FILLER IS NULL THEN
       vIMPCASH:=NVL(TO_NUMBER(TRIM(pMCASHB_P48_ACTC)),0);
    ELSE
       vIMPCASH:=NVL(TO_NUMBER(TRIM(SUBSTR(pP4899FILLER,14,10))),0);
    END IF;

    IF vIMPCASH > 0 THEN -- INDICADOR DE CASHBACK
       vNUMCASH:=1;
    END IF;
    RETURN vNUMCASH;
END;

END;
/


GRANT EXECUTE ON SGCVNZ.PQPCLR_HORA TO ROLE_SOPTECN;

