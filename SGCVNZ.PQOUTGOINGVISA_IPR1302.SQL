CREATE OR REPLACE PACKAGE SGCVNZ.PQOUTGOINGVISA IS

/******* Proyecto : Procesadora Venezuela - Proposito: Outgoing para Visa *******
 Persona                  Fecha           Comentarios
 --------------------     -----------     ------------
 Jose Carbajal Flores     11/Abr/2007     Inicio de Outgoing Visa
 Jose Carbajal Flores     04/Mar/2008     Ultima Modificacion
 Lilly C¿rdova Florentini 05/Nov/2008     Modificacion a estructuras
 Francisco Vasquez        09/Mar/2020     SP de Naiguata Visa
 ********************************************************************************/

 /* modificaciones de Mandatory VISA 2041 Felipe Vitale 9-4-2014
 2.1 Changes to MerchantCategory Codes
 2.10 New Visa Infinite Business Product and Interchange Fee Programs
 2.11 BASE II Draft Data, TCR 5?Payment Service Data
Visa will continue to include the appropriate value in the Spend Qualified Indicator field in
BASE II Draft Data, TCR 5, position 149, prior to delivery to the destination. There are no
changes to the existing BASE II Draft Data record layout.
The existing spend qualified indicator value N and new value Q will continue to be
determined by Visa based on account-level information that resides in Visa systems.
*/

  vCod_EntAdq       VARCHAR2(2):='';
  vDesc_EntAdq      VARCHAR2(20):='';
  vCodMoneda        VARCHAR2(3):='';

  nIdProceso        NUMBER:=0;
  vMensaje          CHAR(100):='';

 TYPE TCR_0 IS RECORD
     (Trn_Code            CHAR(2)       --   1-  2  Transaction Code
    , Trn_Code_Qlf        CHAR(1)       --   3-  3  Transaction Code Qu8alifier
    , Trn_Comp_Seq_Num    CHAR(1)       --   4-  4  Transaction Component Sequence Number
    , Account_Num         CHAR(19)      --   5- 20  Account Number
    --, Account_Num_Ext     CHAR(3)       --  21- 23  Account Number Extension
    , Floor_Limit_Ind     CHAR(1)       --  24- 24  Floor Limit Indicator
    , CRB_Excep_File_Ind  CHAR(1)       --  25- 25  CRB/Exception File Indicator
    , PCAS_ind            CHAR(1)       --  26- 26  Positive Cardholder Authorization Service (PCAS) Indicator
    , Acq_Ref_Num         NUMBER(23)    --  27- 49  Acquirer Reference Number
    , Acq_Bus_ID          NUMBER(8)     --  50- 57  Acquirer's Business ID
    , Purchase_date       CHAR(4)       --  58- 61  Purchase Date (MMDD)
    , Dest_Amount         NUMBER(12)    --  62- 73  Destination Amount
    , Dest_Curren_Code    CHAR(3)       --  74- 76  Destination Currency Code
    , Source_Amount       NUMBER(12)    --  77- 88  Source Amount
    , Source_Curren_Code  CHAR(3)       --  89- 91  Source Currency Code
    , Merch_Name          VARCHAR2(25)  --  92-116  Merchant Name
    , Merch_city          VARCHAR2(13)  -- 117-129  Merchant City
    , Merch_Count_Code    CHAR(3)       -- 130-132  Merchant Country Code
    , Merch_Cate_Code     CHAR(4)       -- 133-136  Merchant Category Code
    , Merch_Zip_Code      CHAR(5)       -- 137-141  Merchant ZIP Code
    , Merch_Prov_Code     CHAR(3)       -- 142-144  Merchant State/Province Code
    , Req_Payment_Serv    CHAR(1)       -- 145-145  Requested Payment Service
    , Reserved_01         CHAR(1)       -- 146-146  Reserved
    , Usage_Code          CHAR(1)       -- 147-147  Usage Code
    , Reason_Code         CHAR(2)       -- 148-149  Requested Reason Code
    , Settlement_Flag     CHAR(1)       -- 150-150  Settlement Flag
    , Aut_Charact_Ind     CHAR(1)       -- 151-151  Authorization Characteristics Indicator
    , Authorization_Code  CHAR(6)       -- 152-157  Authorization Code
    , POS_Capability      CHAR(1)       -- 158-158  POS Terminal Capability
    , Inter_Fee_Ind       CHAR(1)       -- 159-159  International Fee Indicator
    , Cardholder_ID_Met   CHAR(1)       -- 160-160  Cardholder ID Method
    , Collec_Only_Flag    CHAR(1)       -- 161-161  Collection-Only Flag
    , POS_Entry_Mode      CHAR(2)       -- 162-163  POS Entry Mode
    , Central_Proc_Date   CHAR(4)       -- 164-167  Central Processing Date (YDDD)
    , Reimb_Attribute     CHAR(1)       -- 168-168  Reimbursement Attribute
 );

  TYPE TCR_1 IS RECORD
    ( Trn_Code            CHAR(2)      --   1-  2  Transaction Code
    , Trn_Code_Qlf        CHAR(1)      --   3-  3  Transaction Code Qualifier
    , Trn_Comp_Seq_Num    CHAR(1)      --   4-  4  Transaction Component Sequence Number
    , Iss_Workstat_BIN    CHAR(6)      --   5- 10  Issuer Workstation  BIN
    , Acq_Workstat_BIN    CHAR(6)      --  11- 16  Acquirer Workstation BIN
    , Chargeback_Ref_Num  CHAR(6)      --  17- 22  Chargeback Reference Number
    , Documentation_Ind   CHAR(1)      --  23- 23  Documentation Indicator
    , Member_Mess_Text    CHAR(50)     --  24- 73  Member Message Text
    , Special_Cond_Ind    CHAR(2)      --  74- 75  Special Condition Indicator
    , FeeProgramID        CHAR(3):= '   '          --  76 -78  fEE PROGRAM INDICATORS
    , Issuer_charge       CHAR(1):= ' '
    , Reserved_01         CHAR(1):= ' '           --  80  Reserved
    , Card_Acceptor_ID    VARCHAR2(15) --  81- 95  Card Acceptor ID
    , Terminal_ID         VARCHAR2(8)  --  96-103  Terminal ID
    , National_Reimb_Fee  NUMBER(12)   -- 104-115  National Reimbursement Fee
    , Mail_Phone_Ind      CHAR(1)      -- 116-116  Mail/Telephone Indicator
    , Spec_Chargeback_Ind CHAR(1)      -- 117-117  Special Chargeback Indicator
    , Interface_Trace_Num VARCHAR2(6)  -- 118-123  Interface Trace Number
    , Cardhold_ActTer_Ind CHAR(1)      -- 124-124  Cardholder Activated Terminal Indicator
    , Prepaid_Card_Ind    CHAR(1)      -- 125-125  Prepaid Card Indicator
    , Reserved_02         CHAR(1)      -- 126-126  Reserved_02
    , AVS_Response_Code   CHAR(1)      -- 127-127  AVS Response Code
    , Aut_Source_Code     CHAR(1)      -- 128-128  Authorization Source Code
    , Purchase_Iden_Form  CHAR(1)      -- 129-129  Purchase Identifier Format
    , ATM_Account_Select  CHAR(1)      -- 130-130  ATM Account Selection
    , Inst_Payment_Count  CHAR(2)      -- 131-132  Installment Payment Count
    , Purchase_Identifier VARCHAR2(25) -- 133-157  Purchase Identifier
--    , Cashback            NUMBER(9)    -- 158-166  Cashback
    , Cashback            CHAR(9)      -- 158-166  Cashback
    , Chip_Cond_Code      VARCHAR2(1)  -- 167-167  Chip Condition Code
    , POS_Environment     VARCHAR2(1)  -- 168-168  Indicador de Recurrentes
 );

 TYPE TCR_5 IS RECORD
    ( Trn_Code                          CHAR(2)    --  1-->2        Transaction Code
    , Trn_Code_Qlf                      CHAR(1)    --  3-->3        Transaction Code Qualifier
    , Trn_Comp_Seq_Num                  CHAR(1)    --  4-->4        Transaction Component Sequence Number
    , TransactionIdentifier             VARCHAR2(15)   --  5-->19       Transaction Identifier  TID
    , AuthorizedAmount                  CHAR(12)   --  20-->31      Authorized Amount
    , AuthorizationCurrencyCode         CHAR(3)    --  32-->34      Authorization Currency Code
    , AuthorizationResponseCode         CHAR(2)    --  35-->36      Authorization Response Code
    , ValidationCode                    CHAR(4)    --  37-->40      Validation Code
    , ExcludedTranIdenReason            CHAR(1)    --  41-->41      Excluded Transaction Identifier Reason
    , CRSProcessingCode                 CHAR(1)    --  42-->42      CRS Processing Code
    , ChargebackRightsIndicator         CHAR(2)    --  43-->44      Chargeback Rights Indicator
    , MultipleClearingSequenceNumber    CHAR(2)    --  45-->46      Multiple Clearing Sequence Number
    , MultipleClearingSequenceCount     CHAR(2)    --  47-->48      Multiple Clearing Sequence Count
    , MarketSpecificAutDataInd          CHAR(1)    --  49-->49      Market-Specific Authorization Data Indicator
    , TotalAuthorizedAmount             CHAR(12)   --  50-->61      Total Authorized Amount
    , InformationIndicator              CHAR(1)    --  62-->62      Information Indicator
    , MerchantTelephoneNumber           CHAR(14)   --  63-->76      Merchant Telephone Number
    , AdditionalDataIndicator           CHAR(1)    --  77-->77      Additional Data Indicator
    , MerchantVolumeIndicator           CHAR(2)    --  78-->79      Merchant Volume Indicator
    , ElectronicCommGoodsInd            CHAR(2)    --  80-->81      Electronic Commerce Goods Indicator
    , MerchantVerificationValue         CHAR(10)   --  82-->91      Merchant Verification Value
    , InterchangeFeeAmount              CHAR(15)   --  92-->106     Interchange Fee Amount
    , InterchangeFeeSign                CHAR(1)    --  107-->107    Interchange Fee Sign
    , SourceCurrBaseCurrExchRate        CHAR(8)    --  108-->115    Source Currency to Base Currency Exchange Rate
    , BaseCurrtoDestCurrExchRate        CHAR(8)    --  116-->123    Base Currency to Destination Currency Exchange Rate
    , OptionalIssuerISAAmount           CHAR(12)   --  124-->135    Optional Issuer ISA Amount
    , ProductID                         CHAR(2)    --  136-->137    Product ID
    , ProgramID                         CHAR(6)    --  138-->143    Program ID
    , Reserved                          CHAR(24)   --  144-->167    Reserved
    , CVV2ResultCode                    CHAR(1)    --  168-->168    CVV2 Result Code
 );

 TYPE TCR_7 IS RECORD
    ( Trn_Code            CHAR(2)    --   1 -   2   Transaction Code
    , Trn_Code_Qlf        CHAR(1)    --   3 -   3   Transaction Code Qualifier
    , Trn_Comp_Seq_Num    CHAR(1)    --   4 -   4   Transaction Component Sequence Number
    , dctcttcrip          CHAR(2)    --   5 -   6   Transaction Type
    , dcnumsec            NUMBER(3)  --   7 -   9   Card Sequence Number
    , dctcfeccri          CHAR(6)    --   10 -  15  Terminal Transaction Date (YYMMDD)
    , dctccapter          CHAR(6)    --   16 -  21  Terminal Capability Profile
    , dctcpaicri          CHAR(3)    --   22 -  24  Terminal Country Code
    , Num_serie           CHAR(8)    --   25 -  32  Terminal Serial Number
    , dctcnumale          CHAR(8)    --   33 -  40  Unpredictable Number
    , dctcemvatc          CHAR(4)    --   41 -  44  Application Transaction Counter
    , dctcperint          CHAR(4)    --   45 -  48  Application Interchange Profile
    , dctccrippe          CHAR(16)   --   49 -  64  Cryptogram
    , dctcdaplem01        CHAR(2)    --   65 -  66  Issuer Application Data (IAD), byte 2
    , dctcdaplem02        CHAR(2)    --   67 -  68  Issuer Application Data (IAD), byte 3
    , dctcemvtvr          CHAR(10)   --   69 -  78  Terminal Verification Results
    , dctcdaplem03        CHAR(8)    --   79 -  86  Issuer Application Data (IAD), bytes 4¿7
    , dctcimpcri          CHAR(12)   --   87 -  98  Cryptogram Amount
    , dctcdaplem04        CHAR(2)    --   99 - 100  Issuer Application Data (IAD), byte 8
    , dctcdaplem05        CHAR(16)   --   101- 116 Issuer Application Data (IAD), bytes 9¿16
    , dctcdaplem06        CHAR(2)    --   117- 118 Issuer Application Data (IAD), byte 1
    , dctcdaplem07        CHAR(2)    --   119- 120 Issuer Application Data (IAD), byte 17
    , dctcdaplem08        CHAR(30)   --   121- 150 Issuer Application Data (IAD), bytes 18¿32
    , Espacios            CHAR(8)    --   151- 158 Reserved
    , dctcscremi          CHAR(10)   --   159- 168 Issuer Script 1 Results
 );

 TYPE TCR_10_20 IS RECORD
     (Trn_Code            CHAR(2)      --    1 -  2  Transaction Code
    , Trn_Code_Qlf        CHAR(1)      --    3 -  3  Transaction Code Qualifier
    , Trn_Comp_Seq_Num    CHAR(1)      --    4 -  4  Transaction Component Sequence Number
    , Destination_BIN      CHAR(6)       --    5 - 10  Destination BIN
    , Source_BIN          CHAR(6)       --    11 - 16 Soude BIN
    , Reason_Code         CHAR(4)      --     17 - 20 Requested Reason Code
    , Country_Code        CHAR(3)      --     21 - 23 Requested Reason Code
    , Event_Date          CHAR(4)      --  24 - 27 Requested Reason Code
--    , Account_Num         CHAR(16)     --  28 - 43 Account Number
    , Account_Num         CHAR(19)     --  28 - 43 Account Number
    , Account_Num_Ext     CHAR(3)      --  44 - 46 Account Number Extension
    , Dest_Amount         NUMBER(12)   --  47 - 58 Destination Amount
    , Dest_Curren_Code    CHAR(3)      --  59 - 61 Destination Currency Code
    , Source_Amount       NUMBER(12)   --  62 - 73 Source Amount
    , Source_Curren_Code  CHAR(3)      --  74 - 76 Source Currency Code
    , Mess_Text              VARCHAR2(70) --  77 - 146  Message Text
    , Settlement_Flag     CHAR(1)      --     147 - 147 Settlement Flag
    , Trans_Identifier    CHAR(15)   --     148 - 162 Transaction Identifier   --  TID
    , Reserved_01         CHAR(1)      --     163 - 163 Reserved
    , Central_Proc_Date   CHAR(4)      --     164 - 167 Central Processing Date (YDDD)
    , Reimb_Attribute     CHAR(1)      --     168 - 168 Reimbursement Attribute
 );

 TYPE TCR_9X IS RECORD
     (Trn_Code            CHAR(2)     --    1 -  2    Transaction Code
    , Trn_Code_Qlf        CHAR(1)     --    3 -  3    Transaction Code Qualifier
       , Trn_Comp_Seq_Num    CHAR(1)     --    4 -  4    Transaction Component Sequence Number
    , BIN                 CHAR(6)        --    5 - 10    BIN
       , Process_Date        VARCHAR2(5)   --  11 - 15   Processing Date
       , Dest_Amount         NUMBER(15)    --  16 - 30   Destination Amount
    , Num_Money_Trns      NUMBER(12)    --  31 - 42   Number of Money Transactions
       , Batch_Num           NUMBER(6)        --    43 - 48   Batch Number
    , Num_TCRs            NUMBER(12)    --     49 - 60   Number of TCR's
    , Reserved_01         CHAR(6)     --     61 - 66   Reserved
       , Center_Batch_ID     CHAR(8)       --     67 - 74   Cneter Batch Id
       , Num_Trns            NUMBER(9)     --  75 - 83   Number of Transacctions
    , Reserved_02         CHAR(18)    --     84 - 101  Reserved
       , Source_Amount       NUMBER(15)    --  102 - 116 Source Amount
    , Reserved_03         CHAR(15)    --     117 - 131 Reserved
    , Reserved_04         CHAR(15)    --     132 - 146 Reserved
    , Reserved_05         CHAR(15)    --     147 - 161 Reserved
    , Reserved_06         CHAR(7)       --     162 - 168 Reserved
 );

 TYPE ACUMULA_BIN IS RECORD
     (Trn_Code              CHAR(2)     --  Transaction Code
    , Usage_Code            CHAR(1)       --  Usage Code
    , Source_Curren_Code    CHAR(3)       --  Source Currency Code
    , Settlement_Flag       CHAR(1)     --  Settlement_Flag
    , Total_Source_Amount   NUMBER        --  Total Source Amount
    , Total_Trans           NUMBER        --  Total Transactions
    , Total_Comisiones      NUMBER        --  Total Comisiones
 );

 TYPE T_bins IS Table Of ACUMULA_BIN;
 tabla_491955   T_bins;
 tabla_423632   T_bins;
 tabla_423617   T_bins;

 Procedure p_OutGoingVisa(pFecha VARCHAR, pBanco VARCHAR2);
 FUNCTION atleastone_TID (
    COD_INT IN VARCHAR2
   )
   RETURN VARCHAR2;
 -- IPR 1302 Proyecto Naiguata Francisco Vasquez 14022020  
 Procedure p_OutGoingVisa_NGTA(pFecha VARCHAR, pBanco VARCHAR2);
 
 PROCEDURE p_OutGoingVisa_rev(pFecha VARCHAR, pBanco VARCHAR2);
 PROCEDURE p_CargaRegRechazados_rev (pCod_EntAdq varchar2,
                                pFechac6 varchar2,
                                pFile in out UTL_FILE.FILE_TYPE,
                                vNum_Tcrs_tc91 in out number,
                                vSource_Amount_tc91 in out number,
                                vNum_Tcrs_tc92 in out number,
                                vSource_Amount_tc92 in out number,
                                vNum_MTrans_tc91 in out number,
                                vNum_Trns_tc91 in out number,
                                vNum_MTrans_tc92 in out number,
                                vNum_Trns_tc92 in out number,
                                vBatch_Number_tc91 in out number,
                                vBatch_Number_tc92 in out number);
PROCEDURE p_CargaRegRechazados_ventas_r (pCod_EntAdq varchar2,
                                       pFechac6 varchar2,
                                       pFile in out UTL_FILE.FILE_TYPE,
                                       vNum_Tcrs_tc91 in out number,
                                       vSource_Amount_tc91 in out number,
                                       vNum_Tcrs_tc92 in out number,
                                       vSource_Amount_tc92 in out number,
                                       vNum_MTrans_tc91 in out number,
                                       vNum_Trns_tc91 in out number,
                                       vNum_MTrans_tc92 in out number,
                                       vNum_Trns_tc92 in out number,
                                       vBatch_Number_tc91 in out number,
                                       vBatch_Number_tc92 in out number);
END;
/

GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SGCBM;
GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SGCBP;
GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SOPTECN;
CREATE OR REPLACE PACKAGE BODY SGCVNZ.PQOUTGOINGVISA
AS

 FUNCTION f_aut_charact_ind(Trans_Code IN VARCHAR2, puns05_p22 IN VARCHAR2,puns06_p22 IN VARCHAR2) RETURN CHAR;
 FUNCTION f_POS_terminal_capability(puns01_22_actc IN VARCHAR2) RETURN CHAR;
 FUNCTION f_international_fee_indicator(Sflag IN VARCHAR, PosEntryMode IN VARCHAR) RETURN CHAR;
 FUNCTION f_settlement_flag(w_binadq IN VARCHAR2, INLOTE_P29_ACTC IN VARCHAR2) RETURN CHAR;
 FUNCTION f_POS_entrymode(puns07p22actc IN VARCHAR2,puns05p22actc IN VARCHAR2) RETURN CHAR;
 FUNCTION f_cardholder_IDMethod(puns08p22actc IN VARCHAR2) RETURN CHAR;
 FUNCTION f_reimbursement_attribute(Sflag IN VARCHAR2, PosEntryMode IN VARCHAR2,INLOTE_P29_ACTC IN VARCHAR2, transCode IN VARCHAR2) RETURN CHAR; --tipcu1_p46_actc
 FUNCTION f_JulianDate(p12 IN VARCHAR2 ) RETURN CHAR;
 FUNCTION f_usage_code_05(codfun_r_p24_actc IN VARCHAR2)RETURN CHAR;
 FUNCTION f_trn_code(IDEMEN_P00_ACTC  IN VARCHAR2,CODPRO_P03_ACTC IN VARCHAR2, ORIIDE_P56_ACTC IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_nat_reimbursement_fee_06(sFlag IN VARCHAR2,imcutr_r_p46_actc_1 IN VARCHAR2) RETURN NUMBER;
 FUNCTION f_mail_telephon_indicator_06(punser_P22_actc IN VARCHAR2, codact_p26_actc IN VARCHAR2)RETURN CHAR;
-- FUNCTION f_chip_cond_code(punser_P22_actc IN VARCHAR2, cont IN NUMBER) RETURN CHAR;
  FUNCTION f_chip_cond_code(punser_P22_actc IN VARCHAR2) RETURN CHAR;
 FUNCTION f_chrgback_ref_num(codfun_p24_actc IN VARCHAR, numemi_p95_actc IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_member_message_text(codfun_r_p24_actc IN VARCHAR2, numemi_p95_actc IN VARCHAR2,lonr50_p48_actc IN VARCHAR2, codr50_p48_actc IN VARCHAR2,vtapla_p48_actc IN VARCHAR2, vdiferi_p48_actc VARCHAR2)RETURN CHAR;
 FUNCTION f_reason_code(CODRAZ_P25_ACTC IN VARCHAR2,codfun_r_p24_actc IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_source_amount(impcon_p05_actc NUMBER,imptra_p04_actc NUMBER)RETURN NUMBER;
 FUNCTION f_source_currency_code(impcon_p05_actc NUMBER,moncon_p50_actc VARCHAR2,montra_p49_actc VARCHAR2 )RETURN CHAR;

 FUNCTION f_tc10_20_trn_code(SIGCU1_P46_ACCP  IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_tc10_settlement_flag(INLOTE_P29_ACCP IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_country_code(Reason_Code IN VARCHAR2) RETURN CHAR;
 FUNCTION f_tc10_reason_code(CODRAZ_P25_ACCP IN VARCHAR2) RETURN VARCHAR2;
 FUNCTION f_cashBack(mcashb_p48_actc IN VARCHAR2) RETURN CHAR;
-- FUNCTION f_Account_Num(numtar_p02_accp IN VARCHAR2,codraz_p25_accp IN VARCHAR2, regdat_p72_accp IN VARCHAR) RETURN CHAR;
 FUNCTION f_gen_tcr_9X(vTc VARCHAR2, vNum_MTrans_tc9X NUMBER,
                       vBatch_Number_tc9X NUMBER,vNum_Tcrs_tc9X NUMBER,
                       vNum_Trns_tc9X NUMBER,vSource_Amount_tc9X NUMBER,
                       vProcess_Date VARCHAR2) RETURN VARCHAR2;

 FUNCTION f_NroOper(vFechad DATE, vMoneda VARCHAR2) RETURN NUMBER;
 FUNCTION f_MtoNeto(vFechad DATE, vMoneda VARCHAR2) RETURN NUMBER;
 FUNCTION f_buscap26(codact_p26_actc IN VARCHAR) RETURN VARCHAR;
 FUNCTION f_POS_environment(punsr_p22 in varchar) RETURN VARCHAR;
 FUNCTION f_terminal_ID(vPUNSER_P22_ACTC in varchar, vIDETER_P41_ACTC in varchar, vREFADQ_P31_ACTC  in varchar, vIDEADQ_P32_ACTC  in varchar) RETURN VARCHAR;
 FUNCTION f_Cardhold_ActTer_Ind(vIDETER_P41_ACTC in varchar, vvIDEADQ_P32_ACTC in varchar, vTIPMOV_P48_ACTC in varchar) RETURN VARCHAR;

 PROCEDURE p_inicializa_contadores(vFechaProceso CHAR,vMoneda varchar2);
 PROCEDURE p_update_contadores(vBin VARCHAR, vTc VARCHAR, vUsage_Code VARCHAR,vMoneda VARCHAR,
           vSettlementFlag VARCHAR, vTotal_Source_Amount NUMBER, vTotal_Comisiones NUMBER);
 PROCEDURE p_inserta_10_20(vFecPro CHAR, vBIN VARCHAR2, vTran VARCHAR2, vCurrencyC VARCHAR2, vSett_Flag VARCHAR2, vReasonC VARCHAR2, vSource_Am NUMBER);
 PROCEDURE p_documentation_indicator(p24 varchar2,p31 varchar2,nIdProceso number,Pinc_doc_ind  OUT VARCHAR2);
-- FUNCTION f_Documentation_indicator(codfun_p24_actc IN VARCHAR, codraz_p25_actc IN VARCHAR) RETURN CHAR;
 PROCEDURE p_cargaregrechazados_ventas(pCod_EntAdq varchar2,pFechac6 varchar2,pFile in out UTL_FILE.FILE_TYPE,
           vNum_Tcrs_tc91 in out number, vSource_Amount_tc91 in out number,
           vNum_Tcrs_tc92 in out number, vSource_Amount_tc92 in out number,
           vNum_MTrans_tc91 in out number, vNum_Trns_tc91 in out number,
           vNum_MTrans_tc92 in out number, vNum_Trns_tc92 in out number,
           vBatch_Number_tc91 in out number, vBatch_Number_tc92 in out number);
 PROCEDURE p_CargaRegRechazados(pCod_EntAdq varchar2,pFechac6 varchar2,pFile in out UTL_FILE.FILE_TYPE,
           vNum_Tcrs_tc91 in out number, vSource_Amount_tc91 in out number,
           vNum_Tcrs_tc92 in out number, vSource_Amount_tc92 in out number,
           vNum_MTrans_tc91 in out number, vNum_Trns_tc91 in out number,
           vNum_MTrans_tc92 in out number, vNum_Trns_tc92 in out number,
           vBatch_Number_tc91 in out number, vBatch_Number_tc92 in out number);

 PROCEDURE p_OutGoingVisa(pFecha VARCHAR, pBanco VARCHAR2)
 IS

 kSCREMI_P55_ACTC_DEF CHAR(10):= '          ';
 kDirOUT_Alias    VARCHAR2(25):='DIR-OUT';
 kDirOUT_Alias2   VARCHAR2(100):=STD.F_GETVALPAR('DIR-OUT');
 vFileOutVi          VARCHAR2(21);
 vFileOutVi1       VARCHAR2(21);

 file_id1         UTL_FILE.FILE_TYPE;

 pR_CodRes        VARCHAR2(10):='0000';
 pR_DesRes        VARCHAR2(200):= NULL;

 vTCR_0           TCR_0;
 vTCR_1           TCR_1;
 vTCR_5           TCR_5;
 vTCR_10_20       TCR_10_20;
 vTCR_7           TCR_7;

-- cont             NUMBER:=0;
 VCUENTA          NUMBER:=0;
 linea            VARCHAR2(500);

 vC               VARCHAR(50);
 vSQLError        NUMBER;
 vSqlErrM         VARCHAR2(200);
 r_Error          EXCEPTION;

 vFechad          DATE;
 vFechac          CHAR(8);
 vFechac6         CHAR(6);
 vFecha10         DATE;

 vCtaSelec        VARCHAR2(1);
 vNroOperBol      NUMBER:=0;
 vMtoNetoBol      NUMBER:=0;

 --Variables para generacion de TC91/92
 vNum_MTrans_tc91     NUMBER:=0;
 vBatch_Number_tc91   NUMBER:=0;
 vNum_Tcrs_tc91       NUMBER:=0;
 vNum_Trns_tc91       NUMBER:=0;
 vSource_Amount_tc91  NUMBER:=0;
 vNum_MTrans_tc92     NUMBER:=0;
 vBatch_Number_tc92   NUMBER:=0;
 vNum_Tcrs_tc92       NUMBER:=0;
 vNum_Trns_tc92       NUMBER:=0;
 vSource_Amount_tc92  NUMBER:=0;

 varCashback          CHAR(9);

 --TCR_5 y TCR_7
 VTTCRIP_P55_ACTC CLR_DCEMVFULL.ttcrip_p55_actc%TYPE;
 VNUMSEC_P23_ACTC CLR_DCEMVFULL.numsec_p23_actc%TYPE;
 VFECCRI_P55_ACTC CLR_DCEMVFULL.feccri_p55_actc%TYPE;
 VCAPTER_P55_ACTC CLR_DCEMVFULL.capter_p55_actc%TYPE;
 VPAICRI_P55_ACTC CLR_DCEMVFULL.paicri_p55_actc%TYPE;
 VNUMALE_P55_ACTC CLR_DCEMVFULL.numale_p55_actc%TYPE;
 VEMVATC_P55_ACTC CLR_DCEMVFULL.emvatc_p55_actc%TYPE;
 VPERINT_P55_ACTC CLR_DCEMVFULL.perint_p55_actc%TYPE;
 VCRIPPE_P55_ACTC CLR_DCEMVFULL.crippe_p55_actc%TYPE;
 VDAPLEM_P55_ACTC CLR_DCEMVFULL.daplem_p55_actc%TYPE;
 VEMVTVR_P55_ACTC CLR_DCEMVFULL.emvtvr_p55_actc%TYPE;
 VIMPCRI_P55_ACTC CLR_DCEMVFULL.impcri_p55_actc%TYPE;
 VSCREMI_P55_ACTC CLR_DCEMVFULL.scremi_p55_actc%TYPE;
 VRESCRI_P55_ACTC CLR_DCEMVFULL.rescri_p55_actc%TYPE;

 -- Tablas EX8001, COPAE8001
 CURSOR Cr_EX8001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACTC,LONTAR_L02_ACTC,NUMTAR_P02_ACTC,
         CODPRO_P03_ACTC,IMPTRA_P04_ACTC,IMPCON_P05_ACTC,IDETRA_P11_ACTC,TIMLOC_P12_ACTC,
         CODPAI_P19_ACTC,PUNSER_P22_ACTC,CODFUN_P24_ACTC,CODRAZ_P25_ACTC,
         CODACT_P26_ACTC,SESION_P28_ACTC,REFADQ_P31_ACTC,IDEADQ_P32_ACTC,
         NUMAUT_P38_ACTC,CODACR_P39_ACTC,IDETER_P41_ACTC,IDEEST_P42_ACTC,
         NOMEST_P43_ACTC,LOCEST_P43_ACTC,PAIEST_P43_ACTC,TIPCU1_P46_ACTC,SIGCU1_P46_ACTC,
         IMPCU1_P46_ACTC,TIPCU2_P46_ACTC,SIGCU2_P46_ACTC,IMPCU2_P46_ACTC,
         TIPCU3_P46_ACTC,SIGCU3_P46_ACTC,IMPCU3_P46_ACTC,TIPCU4_P46_ACTC,
         SIGCU4_P46_ACTC,IMPCU4_P46_ACTC,CODR13_P48_ACTC,BINADQ_P48_ACTC,
         VTAPLA_P48_ACTC,FILLER_P48_ACTC,MONTRA_P49_ACTC,LONR50_P48_ACTC,
         CODR50_P48_ACTC,DIFERI_P48_ACTC,TIPMOV_P48_ACTC,MONCON_P50_ACTC,
         ORIIDE_P56_ACTC,ORITIM_P56_ACTC,NUMEMI_P95_ACTC,MCASHB_P48_ACTC,
         TIPTRA_P48_ACTC,INLOTE_P29_ACTC
         ,FILLER_P62_ACTC  as TransactionIdentifier   -- pto 2.3 carta tecnica abril 2013 agregar el TID
         --,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACTC,'YYMMDD'),'YYYY')) idparticion
          ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY'))||SUBSTR(sesion_p28_actc,3,2)||TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
          ,TO_NUMBER(TO_CHAR(TO_DATE(SESION_P28_ACTC,'YYMMDD'),'MM')) id_mes
    FROM CLR_EX8001
   WHERE SESION_P28_ACTC = vFechac6
     AND SUBSTR(IDEADQ_P32_ACTC,3,4) = pBanco;

 CURSOR Cr_CLR_COPAE8001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACCP,LONTAR_L02_ACCP,NUMTAR_P02_ACCP,
         IDETRA_P11_ACCP,TIMLOC_P12_ACCP,CODFUN_P24_ACCP,CODRAZ_P25_ACCP,
         SESION_P28_ACCP,INLOTE_P29_ACCP,IDEADQ_L32_ACCP,IDEADQ_P32_ACCP,
         IDEPRE_L33_ACCP,IDEPRE_P33_ACCP,CODACR_P39_ACCP,LONCTR_P46_ACCP,
         TIPCU1_P46_ACCP,SIGCU1_P46_ACCP,IMPCU1_P46_ACCP,TIPCU2_P46_ACCP,
         SIGCU2_P46_ACCP,IMPCU2_P46_ACCP,TIPCU3_P46_ACCP,SIGCU3_P46_ACCP,
         IMPCU3_P46_ACCP,TIPCU4_P46_ACCP,SIGCU4_P46_ACCP,IMPCU4_P46_ACCP,
         ORIDAT_L56_ACCP,ORIIDE_P56_ACCP,ORITRA_P56_ACCP,ORITIM_P56_ACCP,
         ORIADQ_L56_ACCP,ORIADQ_P56_ACCP,NUMMEN_P71_ACCP,REGDAT_L72_ACCP,
         REGDAT_P72_ACCP,TEXTO_L104_ACCP,TEXTO_P104_ACCP
        -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'YYYY')) idparticion
         ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'YYYY'))||SUBSTR(SESION_P28_ACCP,3,2)||TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
         ,TO_NUMBER(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'MM')) id_mes
    FROM CLR_COPAE8001
   WHERE SESION_P28_ACCP = vFechac6
     AND SUBSTR(IDEADQ_P32_ACCP,3,4) = pBanco;

 rCr_EX8001    Cr_EX8001%ROWTYPE;
 rCr_COPAE8001 Cr_CLR_COPAE8001%ROWTYPE;

 BEGIN

  SELECT Cod_EntAdq INTO vCod_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;

  nIdProceso:= PQMONPROC.InsMonProc('POUTVI'||vCod_Entadq);
  PQMONPROC.InsLog(nIdProceso,'M','Se inicia Proceso Outgoing Visa - ' || vCod_EntAdq);

  IF nvl(pBanco,0)= 0 THEN
     vSqlErrM := 'La entidad no puede ser Nulo!';
     RAISE r_error;
  END IF;

  vFechac  := pFecha;
    dbms_output.put_line('vFechac:'||vFechac);
  vFechac6 := SUBSTR(pFecha,3,6);
  vFechad  := to_date(vFechac,'YYYYMMDD');
  vFecha10 := to_date(substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4), 'DD/MM/YYYY');

  vCodMoneda:= PQCOMERCIOS.GCW_F_GETMONEDAVIG(vFecha10);

  dbms_output.put_line('vCodMoneda:'||vCodMoneda);

  SELECT C00NREDU INTO vDesc_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;


  dbms_output.put_line('vDesc_EntAdq:'||vDesc_EntAdq);

  vNroOperBol := f_NroOper(vFechad,vCodMoneda);
  vMtoNetoBol := f_MtoNeto(vFechad,vCodMoneda);
  p_Inicializa_Contadores(vFechac,vCodMoneda);

  vFileOutVi :='OUTVI'|| pBanco || vFechac||'.DAT' ;
  dbms_output.put_line('vFileOutVi:'||vFileOutVi);
  vFileOutVi1 :='OUTVI'|| pBanco || vFechac||'.TXT' ;
    dbms_output.put_line('vFileOutVi1:'||vFileOutVi1);
  PQMONPROC.inslog(nIdProceso,'M','Fecha de Proceso: ' || substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4));
  vC :=pqOutput.open_file(vFileOutVi,kDirOut_Alias);
    dbms_output.put_line('vC:'||vC);

  IF vC <>'OK' THEN
     pR_CodRes:='9003';
     pR_DesRes:='ERR-APL ARCHIVO '||RPAD(vFileOutVi,25,' ')||' NO SE PUEDE ABRIR';
      dbms_output.put_line('****************************************************************************************');
      dbms_output.put_line('**** ' || pR_DesRes || ' .... ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO VISANET ');
      dbms_output.put_line('****************************************************************************************');
     RAISE r_error;
  ELSE
     PQMONPROC.InsLog(nIdProceso,'M','Se inicia Generacion de Outgoing Visa: ' || vFileOutVi || ' a las ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
      dbms_output.put_line('**** INICIA  GENERACION DE OUTGOING VISA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
  END IF;

  file_id1 := utl_file.FOPEN( kDirOUT_Alias2, vFileOutVi1, 'W' );

  OPEN Cr_EX8001;
     dbms_output.put_line('**** ABRIENDO CURSOR EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     PQMONPROC.InsLog(nIdProceso,'M','Abriendo Cursor EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     -- Se carga primero los registros Rechazados
     p_CargaRegRechazados_ventas(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
     BEGIN

     FETCH Cr_EX8001 INTO  rCr_EX8001;
      EXIT WHEN Cr_EX8001%NOTFOUND;
           IF rCr_EX8001.CODPRO_P03_ACTC IN ('000000','010000','200000') AND
              (rCr_EX8001.IDEMEN_P00_ACTC = '1240' OR (rCr_EX8001.IDEMEN_P00_ACTC = '1440' AND rCr_EX8001.REFADQ_P31_ACTC is not null)) THEN
            /*  IF SUBSTR(rCr_EX8001.PUNSER_P22_ACTC, 7, 1) IS NOT NULL THEN
                 cont := cont+1;
              END IF;    */
              vTCR_0.Trn_Code := f_Trn_Code(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC, rCr_EX8001.ORIIDE_P56_ACTC);
                  dbms_output.put_line('vTCR_0.Trn_Code:'||vTCR_0.Trn_Code);
              vTCR_0.Trn_Code_Qlf := '0';
              vTCR_0.Trn_Comp_Seq_Num := '0';
              vTCR_0.Account_Num := rpad(SUBSTR(rCr_EX8001.NUMTAR_P02_ACTC,1,19),19,'0');
                                                dbms_output.put_line('vTCR_0.Account_Num:'||vTCR_0.Account_Num);
              vTCR_0.Floor_Limit_Ind:=' ';
              vTCR_0.CRB_Excep_File_Ind := ' ';
              vTCR_0.PCAS_ind := ' ';
              vTCR_0.Acq_Ref_Num := rCr_EX8001.REFADQ_P31_ACTC;
                                  dbms_output.put_line('vTCR_0.Acq_Ref_Num:'||vTCR_0.Acq_Ref_Num);
              vTCR_0.Acq_Bus_ID := LPAD('0',8,'0'); --'00000000';
              IF (rCr_EX8001.ORITIM_P56_ACTC IS NULL) OR (rCr_EX8001.ORITIM_P56_ACTC = '000000000000') THEN
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,3,4);
              ELSE
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX8001.ORITIM_P56_ACTC,3,4);
              END IF;
              vTCR_0.Dest_Amount := LPAD('0',12,'0'); --'000000000000';
              vTCR_0.Dest_Curren_Code := LPAD(' ',3,' '); --'   ';
              vTCR_0.Source_Amount :=  f_Source_Amount(rCr_EX8001.impcon_p05_actc ,rCr_EX8001.imptra_p04_actc);
              vTCR_0.Source_Curren_Code := f_Source_Currency_Code(rCr_EX8001.impcon_p05_actc ,rCr_EX8001.moncon_p50_actc ,rCr_EX8001.montra_p49_actc);
              IF ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)) >= '3000' AND ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)) <= '3299' THEN
                 vTCR_0.Merch_Name := f_buscap26(rCr_EX8001.CODACT_P26_ACTC) || '             ';
              ELSE
                 vTCR_0.Merch_Name := RPAD(ltrim(rtrim(rCr_EX8001.NOMEST_P43_ACTC)),25,' ');
              END IF;
              vTCR_0.Merch_city := NVL(RPAD(ltrim(rtrim(rCr_EX8001.LOCEST_P43_ACTC)),13,' '),'             ');
               vTCR_0.Merch_Count_Code := RPAD(ltrim(rtrim(rCr_EX8001.PAIEST_P43_ACTC)),3,' ');
              vTCR_0.Merch_Cate_Code  := NVL(ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)),'    ');
              vTCR_0.Merch_Zip_Code   := LPAD('0',5,'0'); --'00000';
              vTCR_0.Merch_Prov_Code  := LPAD(' ',3,' '); --'   ';
              vTCR_0.Req_Payment_Serv :=' ';
              vTCR_0.Reserved_01 := ' ' ;
              vTCR_0.Usage_Code := f_Usage_Code_05(rCr_EX8001.codfun_p24_actc);
              vTCR_0.Reason_Code := f_Reason_Code(rCr_EX8001.CODRAZ_P25_ACTC,rCr_EX8001.codfun_p24_actc);
              vTCR_0.Settlement_Flag  := f_settlement_flag(rCr_EX8001.BINADQ_P48_ACTC, rCr_EX8001.INLOTE_P29_ACTC);
                vTCR_0.Aut_Charact_Ind := ' '; --f_Aut_Charact_Ind(vTCR_0.Trn_Code, SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,5,1) ,SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,6,1));
              IF rCr_EX8001.NUMAUT_P38_ACTC is not null THEN
                 vTCR_0.Authorization_Code := rpad(LTRIM(RTRIM(rCr_EX8001.NUMAUT_P38_ACTC)),6,' ');
              ELSE
                 vTCR_0.Authorization_Code := lpad(' ',6,' ');
              END IF;
              vTCR_0.POS_Capability  := f_Pos_Terminal_Capability(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,1,1));
               vTCR_0.Cardholder_ID_Met := f_Cardholder_IDMethod(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,8,1));
              vTCR_0.Collec_Only_Flag :=' ';
              vTCR_0.POS_Entry_Mode := f_POS_EntryMode(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,7,1),SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,5,1));
              vTCR_0.Inter_Fee_Ind := f_international_fee_indicator(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode);
              IF vTCR_0.Trn_Code = '25' THEN
                 vTCR_0.Central_Proc_Date := f_JulianDate(SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,1,6));
              ELSE
                 vTCR_0.Central_Proc_Date := '0000'; --f_JulianDate(SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,1,6));
              END IF;
              vTCR_0.Reimb_Attribute := f_reimbursement_attribute(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode, rCr_EX8001.INLOTE_P29_ACTC, vTCR_0.Trn_Code);

              linea := LPAD(vTCR_0.Trn_Code,2,'0')|| vTCR_0.Trn_Code_Qlf || vTCR_0.Trn_Comp_Seq_Num ||
                       vTCR_0.Account_Num || vTCR_0.Floor_Limit_Ind ||vTCR_0.CRB_Excep_File_Ind  ||
                       vTCR_0.PCAS_ind || vTCR_0.Acq_Ref_Num ||lpad(vTCR_0.Acq_Bus_ID,8,'0') ||
                       vTCR_0.Purchase_date || LPAD(vTCR_0.Dest_Amount,12,'0') ||vTCR_0.Dest_Curren_Code ||
                       LPAD(to_char(vTCR_0.Source_Amount),12,'0') || vTCR_0.Source_Curren_Code ||
                       vTCR_0.Merch_Name || vTCR_0.Merch_city || vTCR_0.Merch_Count_Code ||
                       vTCR_0.Merch_Cate_Code || vTCR_0.Merch_Zip_Code || vTCR_0.Merch_Prov_Code ||
                       vTCR_0.Req_Payment_Serv || vTCR_0.Reserved_01 || vTCR_0.Usage_Code ||
                       lpad(vTCR_0.Reason_Code,2,'0') || vTCR_0.Settlement_Flag || vTCR_0.Aut_Charact_Ind ||
                       vTCR_0.Authorization_Code || vTCR_0.POS_Capability || vTCR_0.Inter_Fee_Ind ||
                       vTCR_0.Cardholder_ID_Met || vTCR_0.Collec_Only_Flag || vTCR_0.POS_Entry_Mode ||
                       vTCR_0.Central_Proc_Date || vTCR_0.Reimb_Attribute;
                    dbms_output.put_line('lineea1:'||linea);
              pqOutput.PUT_LINE(linea);
          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              p_Update_Contadores(SUBSTR(vTCR_0.Acq_Ref_Num,2,6), vtcr_0.Trn_Code, vtcr_0.Usage_Code, vtcr_0.Source_Curren_Code, vtcr_0.Settlement_Flag, vtcr_0.Source_Amount, rCr_EX8001.IMPCU1_P46_ACTC);
          --  Contadores para TC1
              vNum_Tcrs_tc91:= vNum_Tcrs_tc91+1;
              vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_0.Source_Amount;
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;
              vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_0.Source_Amount;

  -------------------------------------------------TCR1----------------------------------

              vCtaSelec := ' ';
              IF SUBSTR(rCr_EX8001.FILLER_P48_ACTC, 6, 3) = vCodMoneda THEN
                 vCtaSelec := '0';
              END IF;
              IF SUBSTR(rCr_EX8001.FILLER_P48_ACTC, 6, 3) = '840' THEN
                 vCtaSelec := '1';
              END IF;

              vTCR_1.Trn_Code := f_Trn_Code(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC, rCr_EX8001.ORIIDE_P56_ACTC);
              vTCR_1.Trn_Code_Qlf :='0';
              vTCR_1.Trn_Comp_Seq_Num :='1';
              vTCR_1.Iss_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Acq_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Chargeback_Ref_Num := f_Chrgback_Ref_Num(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.NUMEMI_P95_ACTC);
              --vTCR_1.Documentation_Ind := f_Documentation_Indicator(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.CODRAZ_P25_ACTC);
              p_Documentation_indicator(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.refadq_p31_actc, nIdProceso, vTCR_1.Documentation_Ind);
              vTCR_1.Member_Mess_Text := f_member_message_text(rCr_EX8001.CODFUN_P24_ACTC, rCr_EX8001.NUMEMI_P95_ACTC, rCr_EX8001.LONR50_P48_ACTC, rCr_EX8001.CODR50_P48_ACTC, rCr_EX8001.VTAPLA_P48_ACTC, rCr_EX8001.DIFERI_P48_ACTC);
              vTCR_1.Special_Cond_Ind := LPAD(' ',2,' '); --'  '
              vTCR_1.FeeProgramID    := LPAD(' ',3,' '); --'  '
              vTCR_1.Issuer_charge := LPAD(' ',1,' ');
              vTCR_1.Reserved_01 := LPAD(' ',1,' '); --'     '
              vTCR_1.Card_Acceptor_ID := rpad(rCr_EX8001.IDEEST_P42_ACTC, 15, ' ');--rpad((substr(rCr_EX8001.IDEEST_P42_ACTC,3,3) || substr(rCr_EX8001.IDEEST_P42_ACTC,10,6)) || vCtaSelec, 15, ' ');
              vTCR_1.Terminal_ID := f_Terminal_ID(rCr_EX8001.PUNSER_P22_ACTC, rCr_EX8001.IDETER_P41_ACTC, rCr_EX8001.REFADQ_P31_ACTC, rCr_EX8001.IDEADQ_P32_ACTC);
              --vTCR_1.Terminal_ID := rCr_EX8001.IDETER_P41_ACTC;--Cambio pedido de AV 06 SEP 2004
              vTCR_1.National_Reimb_Fee := f_Nat_Reimbursement_fee_06(VTCR_0.Settlement_Flag,rCr_EX8001.IMPCU1_P46_ACTC);
              vTCR_1.Mail_Phone_Ind := f_mail_telephon_indicator_06(rCr_EX8001.PUNSER_P22_ACTC,rCr_EX8001.CODACT_P26_ACTC);
              vTCR_1.Spec_Chargeback_Ind := ' ';
              vTCR_1.Interface_Trace_Num := LPAD('0',6,'0'); --'000000';
              --vTCR_1.Cardhold_ActTer_Ind := ' '; --comentado pedido AV
              vTCR_1.Cardhold_ActTer_Ind := f_Cardhold_ActTer_Ind(rCr_EX8001.IDETER_P41_ACTC, rCr_EX8001.IDEADQ_P32_ACTC, rCr_EX8001.TIPMOV_P48_ACTC);
              vTCR_1.Prepaid_Card_Ind := ' ';
              vTCR_1.Reserved_02 := ' ';
              vTCR_1.AVS_Response_Code := ' ';
              vTCR_1.Aut_Source_Code := ' ';
              vTCR_1.Purchase_Iden_Form := ' ';
              vTCR_1.ATM_Account_Select := '0';
              vTCR_1.Inst_Payment_Count := '  ';
              vTCR_1.Purchase_Identifier := LPAD(' ',25,' '); --'                         ';
              varCashback := NVL(LPAD(rCr_EX8001.MCASHB_P48_ACTC, 9, '0'), '000000000');
              vTCR_1.Chip_Cond_Code := f_Chip_Cond_Code(rCr_EX8001.PUNSER_P22_ACTC);
              vTCR_1.POS_Environment  := f_POS_Environment(rCr_EX8001.PUNSER_P22_ACTC);

              linea := LPAD(vTCR_1.Trn_Code,2,'0') ||   LPAD(vTCR_1.Trn_Code_Qlf,1,'0') ||  vTCR_1.Trn_Comp_Seq_Num ||
                       vTCR_1.Iss_Workstat_BIN || vTCR_1.Acq_Workstat_BIN ||   lpad(vTCR_1.Chargeback_Ref_Num,6,'0') ||
                       vTCR_1.Documentation_Ind || rpad(vTCR_1.Member_Mess_Text,50,' ') ||   vTCR_1.Special_Cond_Ind ||
                       vTCR_1.FeeProgramID || vTCR_1.Issuer_charge ||
                       vTCR_1.Reserved_01 ||  vTCR_1.Card_Acceptor_ID ||   rpad(vTCR_1.Terminal_ID,8,' ') ||
                       lpad(vTCR_1.National_Reimb_Fee,12,'0') ||  vTCR_1.Mail_Phone_Ind ||   vTCR_1.Spec_Chargeback_Ind ||
                       lpad(vTCR_1.Interface_Trace_Num,6,'0') ||  vTCR_1.Cardhold_ActTer_Ind ||   vTCR_1.Prepaid_Card_Ind ||
                       vTCR_1.Reserved_02 ||  vTCR_1.AVS_Response_Code ||   vTCR_1.Aut_Source_Code ||
                       vTCR_1.Purchase_Iden_Form || vTCR_1.ATM_Account_Select ||  vTCR_1.Inst_Payment_Count ||
                       vTCR_1.Purchase_Identifier || varCashback ||  vTCR_1.Chip_Cond_Code ||  vTCR_1.POS_Environment;
                    dbms_output.put_line('lineea2:'||linea);
              pqOutput.PUT_LINE(linea);

          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  Contadores para tc91 y 92 del Tc07
              vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's (Se agrega)
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's (Se agrega)

-----------------
--vTCR_5 y vTCR_7
-----------------

              IF SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,7,1)='5' THEN

                 SELECT COUNT(1)
                   INTO VCUENTA
                   FROM CLR_DCEMVFULL
                  WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX8001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                    AND IDETRA_P11_ACTC = rCr_EX8001.IDETRA_P11_ACTC
                    AND TIMLOC_P12_ACTC = rCr_EX8001.TIMLOC_P12_ACTC
                    AND IDEADQ_P32_ACTC = rCr_EX8001.IDEADQ_P32_ACTC
                    AND  idparticion             =rCr_EX8001.idparticion   /* Implementaci¿n del particionamiento TST 19/04/2014 */
                    AND  id_mes                 =rCr_EX8001.id_mes ;  /* Implementaci¿n del particionamiento TST 19/04/2014 */

                 IF VCUENTA > 0  THEN

                    SELECT TTCRIP_P55_ACTC,NUMSEC_P23_ACTC,FECCRI_P55_ACTC,CAPTER_P55_ACTC,
                           PAICRI_P55_ACTC,NUMALE_P55_ACTC,EMVATC_P55_ACTC,PERINT_P55_ACTC,
                           CRIPPE_P55_ACTC,DAPLEM_P55_ACTC,EMVTVR_P55_ACTC,IMPCRI_P55_ACTC,
                           SCREMI_P55_ACTC, RESCRI_P55_ACTC
                      INTO VTTCRIP_P55_ACTC,VNUMSEC_P23_ACTC,VFECCRI_P55_ACTC,VCAPTER_P55_ACTC,
                           VPAICRI_P55_ACTC,VNUMALE_P55_ACTC,VEMVATC_P55_ACTC,VPERINT_P55_ACTC,
                           VCRIPPE_P55_ACTC,VDAPLEM_P55_ACTC,VEMVTVR_P55_ACTC,VIMPCRI_P55_ACTC,
                           VSCREMI_P55_ACTC,VRESCRI_P55_ACTC
                      FROM CLR_DCEMVFULL
                     WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX8001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                       AND IDETRA_P11_ACTC = rCr_EX8001.IDETRA_P11_ACTC
                       AND TIMLOC_P12_ACTC = rCr_EX8001.TIMLOC_P12_ACTC
                       AND IDEADQ_P32_ACTC = rCr_EX8001.IDEADQ_P32_ACTC
                       AND  idparticion             =rCr_EX8001.idparticion   /* Implementaci¿n del particionamiento TST 19/04/2014 */
                       AND  id_mes                 =rCr_EX8001.id_mes ;  /* Implementaci¿n del particionamiento TST 19/04/2014 */

                    vTCR_5.Trn_Code                       := f_Trn_Code(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC,rCr_EX8001.ORIIDE_P56_ACTC);       --   1-  2   Transaction Code --LPAD('0',2,'0');
                    vTCR_5.Trn_Code_Qlf                   := '0';
                    vTCR_5.Trn_Comp_Seq_Num               := '5';
                    vTCR_5.TransactionIdentifier          := NVL(substr(rCr_EX8001.TransactionIdentifier,1,15), LPAD('0',15,'0')) ; --TID
                    --dbms_output.put_line('ENTRO 1 : ' || vTCR_5.TransactionIdentifier);
                    vTCR_5.AuthorizedAmount               := LPAD(TO_CHAR(rCr_EX8001.IMPTRA_P04_ACTC),12,'0');  --LPAD('0',12,'0');
                    vTCR_5.AuthorizationCurrencyCode      := LPAD(rCr_EX8001.MONTRA_P49_ACTC,3,' ');  --pSource_Curren_Code; --LPAD(' ',3,' ');
                    dbms_output.put_line('vTCR_5.AuthorizationCurrencyCode:'||vTCR_5.AuthorizationCurrencyCode);
                    vTCR_5.AuthorizationResponseCode      := LPAD(NVL(TRIM(VRESCRI_P55_ACTC),0),2,'0'); --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0');
                    vTCR_5.ValidationCode                 := LPAD(' ',4,' ');
                    vTCR_5.ExcludedTranIdenReason         := LPAD(' ',1,' ');
                    vTCR_5.CRSProcessingCode              := LPAD(' ',1,' ');
                    vTCR_5.ChargebackRightsIndicator      := LPAD(' ',2,' ');
                    vTCR_5.MultipleClearingSequenceNumber := LPAD('0',2,'0');
                    vTCR_5.MultipleClearingSequenceCount  := LPAD('0',2,'0');
                    vTCR_5.MarketSpecificAutDataInd       := LPAD(' ',1,' ');
                    vTCR_5.TotalAuthorizedAmount          := LPAD('0',12,'0');
                    vTCR_5.InformationIndicator           := LPAD(' ',1,' ');
                    vTCR_5.MerchantTelephoneNumber        := LPAD(' ',14,' ');
                    vTCR_5.AdditionalDataIndicator        := LPAD(' ',1,' ');
                    vTCR_5.MerchantVolumeIndicator        := LPAD(' ',2,' ');
                    vTCR_5.ElectronicCommGoodsInd         := LPAD(' ',2,' ');
                    vTCR_5.MerchantVerificationValue      := LPAD(' ',10,' ');
                    vTCR_5.InterchangeFeeAmount           := LPAD('0',15,'0');
                    vTCR_5.InterchangeFeeSign             := LPAD(' ',1,' ');
                    vTCR_5.SourceCurrBaseCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.BaseCurrtoDestCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.OptionalIssuerISAAmount        := LPAD('0',12,'0');
                    vTCR_5.ProductID                      := LPAD(' ',2,' ');
                    vTCR_5.ProgramID                      := LPAD(' ',6,' ');
                    vTCR_5.Reserved                       := LPAD(' ',24,' ');
                    vTCR_5.CVV2ResultCode                 := LPAD(' ',1,' ');


                     IF TRIM(vTCR_5.ProductID) = 'I' THEN
                        vTCR_5.ProductID := 'I1';
                    END IF;

                    linea := LPAD(vTCR_5.Trn_Code,2,'0')||vTCR_5.Trn_Code_Qlf||
                             vTCR_5.Trn_Comp_Seq_Num||vTCR_5.TransactionIdentifier||
                             vTCR_5.AuthorizedAmount||vTCR_5.AuthorizationCurrencyCode||
                             vTCR_5.AuthorizationResponseCode||vTCR_5.ValidationCode||
                             vTCR_5.ExcludedTranIdenReason||vTCR_5.CRSProcessingCode||
                             vTCR_5.ChargebackRightsIndicator||vTCR_5.MultipleClearingSequenceNumber||
                             vTCR_5.MultipleClearingSequenceCount||vTCR_5.MarketSpecificAutDataInd||
                             vTCR_5.TotalAuthorizedAmount||vTCR_5.InformationIndicator||
                             vTCR_5.MerchantTelephoneNumber||vTCR_5.AdditionalDataIndicator||
                             vTCR_5.MerchantVolumeIndicator||vTCR_5.ElectronicCommGoodsInd||
                             vTCR_5.MerchantVerificationValue||vTCR_5.InterchangeFeeAmount||
                             vTCR_5.InterchangeFeeSign||vTCR_5.SourceCurrBaseCurrExchRate||
                             vTCR_5.BaseCurrtoDestCurrExchRate||vTCR_5.OptionalIssuerISAAmount||
                             vTCR_5.ProductID||vTCR_5.ProgramID||
                             vTCR_5.Reserved||
                             vTCR_5.CVV2ResultCode;



                    --dbms_output.put_line('ENTRO 1 linea det:' || linea);

                    pqOutput.PUT_LINE(linea);
                    --Contadores para tc91 y 92 del Tc05
                    vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;
                    vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;


                    vTCR_7.Trn_Code := f_Trn_Code(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC,rCr_EX8001.ORIIDE_P56_ACTC);
                    vTCR_7.Trn_Code_Qlf := '0';
                    vTCR_7.Trn_Comp_Seq_Num := '7';
                    vTCR_7.dctcttcrip := LPAD(NVL(VTTCRIP_P55_ACTC,'0'),2,'0');
                    vTCR_7.dcnumsec := LPAD(NVL(VNUMSEC_P23_ACTC,0),3,'0');
                    vTCR_7.dctcfeccri := LPAD(VFECCRI_P55_ACTC,6,'0');
                    vTCR_7.dctccapter := LPAD(VCAPTER_P55_ACTC,6,'0');
                    vTCR_7.dctcpaicri := '862'; -- RPAD(VPAICRI_P55_ACTC,3,' ');
                    vTCR_7.Num_serie := LPAD (' ',8,' '); --LPAD(rCr_EX8001.IDETER_P41_ACTC,8,' '); --' ';
                    vTCR_7.dctcnumale := LPAD(VNUMALE_P55_ACTC,8,'0');
                    vTCR_7.dctcemvatc := LPAD(VEMVATC_P55_ACTC,4,'0');
                    vTCR_7.dctcperint := LPAD(VPERINT_P55_ACTC,4,'0');
                    vTCR_7.dctccrippe := LPAD(VCRIPPE_P55_ACTC,16,'0');
                    vTCR_7.dctcdaplem01 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),3,2),2,'0');
                    vTCR_7.dctcdaplem02 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),5,2),2,'0');
                    vTCR_7.dctcemvtvr := LPAD(VEMVTVR_P55_ACTC,10,'0');
                    vTCR_7.dctcdaplem03 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),7,8),8,'0');
                    vTCR_7.dctcimpcri := LPAD(VIMPCRI_P55_ACTC,12,'0');
                    vTCR_7.dctcdaplem04 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),15,2),2,'0');
                    vTCR_7.dctcdaplem05 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),17,16),16,'0');
                    vTCR_7.dctcdaplem06 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),1,2),2,'0');
                    vTCR_7.dctcdaplem07 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),33,2),2,'0');
                    vTCR_7.dctcdaplem08 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),35,30),30,'0');
                    vTCR_7.Espacios := lpad(' ',8,' ');
                    vTCR_7.dctcscremi := nvl(LPAD(STD.F_RTRIM4HEX(VSCREMI_P55_ACTC),10,'0'),kSCREMI_P55_ACTC_DEF);

                    linea := LPAD(vTCR_7.Trn_Code,2,'0') || vTCR_7.Trn_Code_Qlf || vTCR_7.Trn_Comp_Seq_Num ||
                             vTCR_7.dctcttcrip || LPAD(vTCR_7.dcnumsec,3,'0')|| vTCR_7.dctcfeccri||vTCR_7.dctccapter||
                             vTCR_7.dctcpaicri||vTCR_7.Num_serie||vTCR_7.dctcnumale||vTCR_7.dctcemvatc||
                             vTCR_7.dctcperint||vTCR_7.dctccrippe||vTCR_7.dctcdaplem01||vTCR_7.dctcdaplem02||
                             vTCR_7.dctcemvtvr||vTCR_7.dctcdaplem03||vTCR_7.dctcimpcri||vTCR_7.dctcdaplem04||
                             vTCR_7.dctcdaplem05||vTCR_7.dctcdaplem06||vTCR_7.dctcdaplem07||vTCR_7.dctcdaplem08||
                             vTCR_7.Espacios||vTCR_7.dctcscremi;

                    dbms_output.put_line('linea:' || linea);
                    pqOutput.PUT_LINE(linea);

                     --  Contadores para tc91 y 92 del Tc07
                     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
                     vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's
                 ELSE
                    IF vTCR_0.Trn_Code = '05' then
                       dbms_output.put_line('Alerta TCR_0 CHIP NO CARGADA:  ' || rCr_EX8001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                       PQMONPROC.InsLog(nIdProceso,'W','Alerta TCR_0 CHIP NO CARGADA: ' || rCr_EX8001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                    END IF;
                 END IF;


              END IF;

          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
          --  **********Para el TC91************
              vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
              vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
              vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
              vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
              IF vNum_MTrans_tc91 = 500 THEN
                 vNum_Tcrs_tc91 := vNum_Tcrs_tc91+1;--Numero de TCR's..91
                 vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's..92
                 vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                 vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                 vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                 vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                 linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                          vBatch_Number_tc91,vNum_Tcrs_tc91,
                          vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                 pqOutput.PUT_LINE(linea);
                 vNum_MTrans_tc91 := 0;
                 vNum_Tcrs_tc91 := 0;
                 vNum_Trns_tc91 := 0;
                 vSource_Amount_tc91 := 0;
              END IF;
           END IF;
  EXCEPTION
     WHEN OTHERS THEN
          vSQLError:=SQLCODE;
          vSqlErrM :=SUBSTR(SQLERRM,1,200);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR EX8001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.InsLog(nIdProceso, 'E', 'INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA','99');
            dbms_output.put_line('******************************************************************************************');
             dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
             dbms_output.put_line('**** PROCESANDO CURSOR EX8001 - ' || vDesc_EntAdq );
             dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA');
             dbms_output.put_line('******************************************************************************************');
          RETURN;
     END;
  END LOOP;
  dbms_output.put_line('**** CERRANDO CURSOR EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));

  CLOSE Cr_EX8001;

  -- Para el TC91

  IF vNum_Tcrs_tc91 >= 0 THEN
 -- las sgtes 4 lineas han sido descomentadas EL 04 MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91, vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     pqOutput.PUT_LINE(linea);
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:=0;
  END If;
-- Fin para TC91

  OPEN Cr_CLR_COPAE8001;
  dbms_output.put_line('**** ABRIENDO CURSOR COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       PQMONPROC.inslog(nIdProceso,'M','Abriendo Cursor COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       p_CargaRegRechazados(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
   BEGIN
      FETCH Cr_CLR_COPAE8001 INTO  rCr_COPAE8001;
      EXIT WHEN Cr_CLR_COPAE8001%NOTFOUND;
         IF rCr_COPAE8001.IDEMEN_P00_ACCP = '1740'  THEN
            vTCR_10_20.Trn_Code := f_tc10_20_Trn_Code(rCr_COPAE8001.SIGCU1_P46_ACCP);
            vTCR_10_20.Trn_Code_Qlf:= '0';
            vTCR_10_20.Trn_Comp_Seq_Num:= '0';
            vTCR_10_20.Destination_BIN := '000000';
            IF SUBSTR(rCr_COPAE8001.IDEADQ_P32_ACCP,3,4) ='0105' THEN
               vTCR_10_20.Source_BIN := '411032';
            ELSIF SUBSTR(rCr_COPAE8001.IDEADQ_P32_ACCP,3,4) ='0108' THEN
               vTCR_10_20.Source_BIN := '420198';
            END IF;
            vTCR_10_20.Reason_Code := f_tc10_Reason_Code(rCr_COPAE8001.CODRAZ_P25_ACCP);
            vTCR_10_20.Country_Code := f_Country_Code(vTCR_10_20.Reason_Code);
               vTCR_10_20.Event_Date:= SUBSTR(rCr_COPAE8001.TIMLOC_P12_ACCP,3,4);
            vTCR_10_20.Account_Num := rpad(ltrim(rtrim(rCr_COPAE8001.numtar_p02_accp)),19,'0');
            IF (ltrim(rtrim(rCr_COPAE8001.numtar_p02_accp)) = '' OR TO_NUMBER(rCr_COPAE8001.numtar_p02_accp)=0 OR rCr_COPAE8001.numtar_p02_accp is null) AND
               (rCr_COPAE8001.codraz_p25_accp = '7703') THEN
               IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0002' THEN
                    vTCR_10_20.Destination_BIN := '450645';
                ELSE
                   IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0003' THEN
                      vTCR_10_20.Destination_BIN := '454775';
                  ELSE
                     IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0011' THEN
                         vTCR_10_20.Destination_BIN := '492112';
                     END IF;
                  END IF;
               END IF;
            END IF;
            vTCR_10_20.Dest_Amount:= 0;
            vTCR_10_20.Dest_Curren_Code := '   ';
            IF rCr_COPAE8001.IMPCU1_P46_ACCP is null THEN
               vTCR_10_20.Source_Amount := 0;
            ELSE
               vTCR_10_20.Source_Amount := rCr_COPAE8001.IMPCU1_P46_ACCP;
            END IF;
            vTCR_10_20.Source_Curren_Code:= vCodMoneda;
            vTCR_10_20.Mess_Text:= RPAD(rCr_COPAE8001.TEXTO_P104_ACCP,70,' ');
            vTCR_10_20.Settlement_Flag := f_tc10_Settlement_flag(rCr_COPAE8001.INLOTE_P29_ACCP);--vTCR_10_20.Source_BIN, rCr_COPAE8001.TIPCU1_P46_ACCP);
            IF SUBSTR(rCr_COPAE8001.numtar_p02_accp,1,16) = '4506450000000016' and rCr_COPAE8001.codraz_p25_accp='7711' AND substr(rCr_COPAE8001.texto_p104_accp,1,14) = 'SECURITIZACION' THEN
               vTCR_10_20.Settlement_Flag := '0';
            END IF;
            vTCR_10_20.Trans_Identifier:=   rCr_EX8001.TransactionIdentifier ; -- 0;
          --  dbms_output.put_line('ENTRO 2 vTCR_10_20.Trans_Identifier : ' || vTCR_10_20.Trans_Identifier);
            vTCR_10_20.Reserved_01:= ' ';
            vTCR_10_20.Central_Proc_Date:= '0000';
            vTCR_10_20.Reimb_Attribute:='0';
            linea := vTCR_10_20.Trn_Code ||  vTCR_10_20.Trn_Code_Qlf ||   vTCR_10_20.Trn_Comp_Seq_Num||
                     lpad(vTCR_10_20.Destination_BIN,6,'0') || lpad(vTCR_10_20.Source_BIN,6,'0') ||   lpad(vTCR_10_20.Reason_Code,4,'0') ||
                     vTCR_10_20.Country_Code || lpad(vTCR_10_20.Event_Date,4,'0') ||   vTCR_10_20.Account_Num ||
                     lpad(vTCR_10_20.Dest_Amount,12,'0') || vTCR_10_20.Dest_Curren_Code ||
                     lpad(vTCR_10_20.Source_Amount,12,'0') || vTCR_10_20.Source_Curren_Code || vTCR_10_20.Mess_Text ||
                     vTCR_10_20.Settlement_Flag || LPAD(vTCR_10_20.Trans_Identifier,15,'0') ||  vTCR_10_20.Reserved_01 ||
                     vTCR_10_20.Central_Proc_Date || vTCR_10_20.Reimb_Attribute;
             --dbms_output.put_line('ENTRO 2 det: '|| linea);
             pqOutput.PUT_LINE(linea);
             utl_file.PUT_LINE( file_id1, linea);
             p_Update_Contadores(vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, '1', vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.Source_Amount, 0);
             p_Inserta_10_20(vFechac, vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, rCr_COPAE8001.CODRAZ_P25_ACCP, vTCR_10_20.Source_Amount);
             --**********Para el TC91************
             vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_10_20.Source_Amount;
             vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_10_20.Source_Amount;
             vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
             vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
             vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
             vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's
             vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
             vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
             IF vNum_MTrans_tc91 = 500 THEN
                vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
                vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
                vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                         vBatch_Number_tc91,vNum_Tcrs_tc91,
                         vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                pqOutput.PUT_LINE(linea);
                vNum_MTrans_tc91 := 0;
                vNum_Tcrs_tc91 :=0;
                vNum_Trns_tc91 := 0;
                vSource_Amount_tc91:= 0;
             END IF;
          END IF;

   EXCEPTION
      WHEN OTHERS THEN
           vSQLError:=SQLCODE;
           vSqlErrM :=SUBSTR(SQLERRM,1,200);
           PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE8001 - ' || vDesc_EntAdq);
           vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
           PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
              dbms_output.put_line('******************************************************************************************');
              dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
              dbms_output.put_line('**** PROCESANDO CURSOR COPAE8001 - Banco: ' || vDesc_EntAdq);
              dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ');
              dbms_output.put_line('******************************************************************************************');
           RETURN;
   END;

  END LOOP;
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  CLOSE Cr_CLR_COPAE8001;
  -- Para el TC91
  IF vNum_Tcrs_tc91 >=0 THEN
     -- LAS SGTES 4 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                            vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:= 0;
     pqOutput.PUT_LINE(linea);
  END IF;
   -- Para el TC92
   -- LAS SGTES 2 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
  vNum_Tcrs_tc92 := vNum_Tcrs_tc92 + 1;
  vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
  vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
  linea := f_gen_TCR_9X('92', vNum_MTrans_tc92 ,
                         vBatch_Number_tc92,vNum_Tcrs_tc92,
                         vNum_Trns_tc92,vSource_Amount_tc92,vFechac6) ;

  pqOutput.PUT_LINE(linea);
   dbms_output.put_line('**** -----------------------------------------------------------------------');
   dbms_output.put_line('**** TOME NOTA DE ESTE NUMERO PARA EL CUADRE CONTRA EL CLEARING ');
   dbms_output.put_line('**** OUTGOING DE VISA : ' || lpad(to_char(vNum_Trns_tc92) || ' TRANSACCIONES',52, ' '));
   dbms_output.put_line('**** -----------------------------------------------------------------------');

  vNum_MTrans_tc92 := 0;
  vNum_Tcrs_tc92 :=0;
  vNum_Trns_tc92 := 0;
  vSource_Amount_tc92:=0;
  vBatch_Number_tc92:=0;

  -- Fin Para el TC92
  -- FIN DE CLR_COPAE8001************************************/
  pqOutput.Close_file;--SE CERRO EL ARCHIVO OUTVIEEEEYYYYMMDD.DAT
  utl_file.fCLOSE(file_id1);-- SE CERRO EL ARCHIVO OUTVIEEEEYYMMDD.TXT
dbms_output.put_line('****************************************************************************');
   dbms_output.put_line('**** TERMINA GENERACION DE OUTGOING VISA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
   dbms_output.put_line('****************************************************************************');

  PQMONPROC.inslog(nIdProceso,'M','Termina Generacion de Outgoing Visa: ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
  vMensaje := pqmonproc.updmonproc (nIdProceso, 'F');
  --FIN DE ARCHIVO OUTVIEEEEYYYYMMDD.DAT ********
 EXCEPTION
     WHEN r_error Then
     dbms_output.put_line('vSqlErrM: ' || vSqlErrM || ' '|| ' pIdProceso:' || nIdProceso);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE8001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
          RETURN;
 END p_OutGoingVisa;

-----------------------------------------------------------------------------------------------------
--<< INICIO P_OUTGOINGVISA_NGTA IPR - 1302 Francisco V�squez>>---------------------------------------
 PROCEDURE p_OutGoingVisa_NGTA(pFecha VARCHAR, pBanco VARCHAR2)
 IS

 kSCREMI_P55_ACTC_DEF CHAR(10):= '          ';
 kDirOUT_Alias    VARCHAR2(25):='DIR-OUT';
 kDirOUT_Alias2   VARCHAR2(100):=STD.F_GETVALPAR('DIR-OUT');
 vFileOutVi          VARCHAR2(21);
 vFileOutVi1       VARCHAR2(21);

 file_id1         UTL_FILE.FILE_TYPE;

 pR_CodRes        VARCHAR2(10):='0000';
 pR_DesRes        VARCHAR2(200):= NULL;

 vTCR_0           TCR_0;
 vTCR_1           TCR_1;
 vTCR_5           TCR_5;
 vTCR_10_20       TCR_10_20;
 vTCR_7           TCR_7;

-- cont             NUMBER:=0;
 VCUENTA          NUMBER:=0;
 linea            VARCHAR2(500);

 vC               VARCHAR(50);
 vSQLError        NUMBER;
 vSqlErrM         VARCHAR2(200);
 r_Error          EXCEPTION;

 vFechad          DATE;
 vFechac          CHAR(8);
 vFechac6         CHAR(6);
 vFecha10         DATE;

 vCtaSelec        VARCHAR2(1);
 vNroOperBol      NUMBER:=0;
 vMtoNetoBol      NUMBER:=0;

 --Variables para generacion de TC91/92
 vNum_MTrans_tc91     NUMBER:=0;
 vBatch_Number_tc91   NUMBER:=0;
 vNum_Tcrs_tc91       NUMBER:=0;
 vNum_Trns_tc91       NUMBER:=0;
 vSource_Amount_tc91  NUMBER:=0;
 vNum_MTrans_tc92     NUMBER:=0;
 vBatch_Number_tc92   NUMBER:=0;
 vNum_Tcrs_tc92       NUMBER:=0;
 vNum_Trns_tc92       NUMBER:=0;
 vSource_Amount_tc92  NUMBER:=0;

 varCashback          CHAR(9);

 --TCR_5 y TCR_7
 VTTCRIP_P55_ACTC CLR_DCEMVFULL.ttcrip_p55_actc%TYPE;
 VNUMSEC_P23_ACTC CLR_DCEMVFULL.numsec_p23_actc%TYPE;
 VFECCRI_P55_ACTC CLR_DCEMVFULL.feccri_p55_actc%TYPE;
 VCAPTER_P55_ACTC CLR_DCEMVFULL.capter_p55_actc%TYPE;
 VPAICRI_P55_ACTC CLR_DCEMVFULL.paicri_p55_actc%TYPE;
 VNUMALE_P55_ACTC CLR_DCEMVFULL.numale_p55_actc%TYPE;
 VEMVATC_P55_ACTC CLR_DCEMVFULL.emvatc_p55_actc%TYPE;
 VPERINT_P55_ACTC CLR_DCEMVFULL.perint_p55_actc%TYPE;
 VCRIPPE_P55_ACTC CLR_DCEMVFULL.crippe_p55_actc%TYPE;
 VDAPLEM_P55_ACTC CLR_DCEMVFULL.daplem_p55_actc%TYPE;
 VEMVTVR_P55_ACTC CLR_DCEMVFULL.emvtvr_p55_actc%TYPE;
 VIMPCRI_P55_ACTC CLR_DCEMVFULL.impcri_p55_actc%TYPE;
 VSCREMI_P55_ACTC CLR_DCEMVFULL.scremi_p55_actc%TYPE;
 VRESCRI_P55_ACTC CLR_DCEMVFULL.rescri_p55_actc%TYPE;

 -- Tablas EX9001, COPAE9001
 CURSOR Cr_EX9001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACTC,LONTAR_L02_ACTC,NUMTAR_P02_ACTC,
         CODPRO_P03_ACTC,IMPTRA_P04_ACTC,IMPCON_P05_ACTC,IDETRA_P11_ACTC,TIMLOC_P12_ACTC,
         CODPAI_P19_ACTC,PUNSER_P22_ACTC,CODFUN_P24_ACTC,CODRAZ_P25_ACTC,
         CODACT_P26_ACTC,SESION_P28_ACTC,REFADQ_P31_ACTC,IDEADQ_P32_ACTC,
         NUMAUT_P38_ACTC,CODACR_P39_ACTC,IDETER_P41_ACTC,IDEEST_P42_ACTC,
         NOMEST_P43_ACTC,LOCEST_P43_ACTC,PAIEST_P43_ACTC,TIPCU1_P46_ACTC,SIGCU1_P46_ACTC,
         IMPCU1_P46_ACTC,TIPCU2_P46_ACTC,SIGCU2_P46_ACTC,IMPCU2_P46_ACTC,
         TIPCU3_P46_ACTC,SIGCU3_P46_ACTC,IMPCU3_P46_ACTC,TIPCU4_P46_ACTC,
         SIGCU4_P46_ACTC,IMPCU4_P46_ACTC,CODR13_P48_ACTC,BINADQ_P48_ACTC,
         VTAPLA_P48_ACTC,FILLER_P48_ACTC,MONTRA_P49_ACTC,LONR50_P48_ACTC,
         CODR50_P48_ACTC,DIFERI_P48_ACTC,TIPMOV_P48_ACTC,MONCON_P50_ACTC,
         ORIIDE_P56_ACTC,ORITIM_P56_ACTC,NUMEMI_P95_ACTC,MCASHB_P48_ACTC,
         TIPTRA_P48_ACTC,INLOTE_P29_ACTC
         ,FILLER_P62_ACTC  as TransactionIdentifier   -- pto 2.3 carta tecnica abril 2013 agregar el TID
         --,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACTC,'YYMMDD'),'YYYY')) idparticion
          ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY'))||SUBSTR(sesion_p28_actc,3,2)||TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
    FROM CLR_EX9001
   WHERE SESION_P28_ACTC = vFechac6
   AND inlote_p29_actc <>'318' -- EXCLUYE TRANSACCIONES CNB 12/09/2017 CB IPR-1113
     AND SUBSTR(IDEADQ_P32_ACTC,3,4) = pBanco;

 CURSOR Cr_CLR_COPAE9001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACCP,LONTAR_L02_ACCP,NUMTAR_P02_ACCP,
         IDETRA_P11_ACCP,TIMLOC_P12_ACCP,CODFUN_P24_ACCP,CODRAZ_P25_ACCP,
         SESION_P28_ACCP,INLOTE_P29_ACCP,IDEADQ_L32_ACCP,IDEADQ_P32_ACCP,
         IDEPRE_L33_ACCP,IDEPRE_P33_ACCP,CODACR_P39_ACCP,LONCTR_P46_ACCP,
         TIPCU1_P46_ACCP,SIGCU1_P46_ACCP,IMPCU1_P46_ACCP,TIPCU2_P46_ACCP,
         SIGCU2_P46_ACCP,IMPCU2_P46_ACCP,TIPCU3_P46_ACCP,SIGCU3_P46_ACCP,
         IMPCU3_P46_ACCP,TIPCU4_P46_ACCP,SIGCU4_P46_ACCP,IMPCU4_P46_ACCP,
         ORIDAT_L56_ACCP,ORIIDE_P56_ACCP,ORITRA_P56_ACCP,ORITIM_P56_ACCP,
         ORIADQ_L56_ACCP,ORIADQ_P56_ACCP,NUMMEN_P71_ACCP,REGDAT_L72_ACCP,
         REGDAT_P72_ACCP,TEXTO_L104_ACCP,TEXTO_P104_ACCP
          -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'YYYY')) idparticion
         ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'YYYY'))||SUBSTR(SESION_P28_ACCP,3,2)||TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion

    FROM CLR_COPAE9001
   WHERE SESION_P28_ACCP = vFechac6
   AND inlote_p29_accp <>'318' -- EXCLUYE TRANSACCIONES CNB 12/09/2017 CB IPR-1113
     AND SUBSTR(IDEADQ_P32_ACCP,3,4) = pBanco;

 rCr_EX9001    Cr_EX9001%ROWTYPE;
 rCr_COPAE9001 Cr_CLR_COPAE9001%ROWTYPE;

 BEGIN

  SELECT Cod_EntAdq
  INTO vCod_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;

  nIdProceso:= PQMONPROC.InsMonProc('POUTVI_NGTA'||vCod_Entadq);
  PQMONPROC.InsLog(nIdProceso,'M','Se inicia Proceso Outgoing Visa-Ngta - ' || vCod_EntAdq);

  IF nvl(pBanco,0)= 0 THEN
     vSqlErrM := 'La entidad no puede ser Nulo!';
     RAISE r_error;
  END IF;

  vFechac  := pFecha;
    dbms_output.put_line('vFechac:'||vFechac);
  vFechac6 := SUBSTR(pFecha,3,6);
  vFechad  := to_date(vFechac,'YYYYMMDD');
  vFecha10 := to_date(substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4), 'DD/MM/YYYY');

  vCodMoneda:= PQCOMERCIOS.GCW_F_GETMONEDAVIG(vFecha10);

  dbms_output.put_line('vCodMoneda:'||vCodMoneda);

  SELECT C00NREDU INTO vDesc_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;


  dbms_output.put_line('vDesc_EntAdq:'||vDesc_EntAdq);

  vNroOperBol := f_NroOper(vFechad,vCodMoneda);
  vMtoNetoBol := f_MtoNeto(vFechad,vCodMoneda);
  p_Inicializa_Contadores(vFechac,vCodMoneda);
  --<< Nuevo Saliente Naiguata IPR - 1302 FJVG  09012020
  vFileOutVi :='OUTVI-NGTA'|| pBanco || vFechac||'.DAT' ;
  dbms_output.put_line('vFileOutVi:'||vFileOutVi);
  vFileOutVi1 :='OUTVI-NGTA'|| pBanco || vFechac||'.TXT' ;
  dbms_output.put_line('vFileOutVi1:'||vFileOutVi1);
  PQMONPROC.inslog(nIdProceso,'M','Fecha de Proceso: ' || substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4));
  vC :=pqOutput.open_file(vFileOutVi,kDirOut_Alias);
    dbms_output.put_line('vC:'||vC);

  IF vC <>'OK' THEN
     pR_CodRes:='9003';
     pR_DesRes:='ERR-APL ARCHIVO '||RPAD(vFileOutVi,25,' ')||' NO SE PUEDE ABRIR';
      dbms_output.put_line('****************************************************************************************');
      dbms_output.put_line('**** ' || pR_DesRes || ' .... ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO NAIGUATA');
      dbms_output.put_line('****************************************************************************************');
     RAISE r_error;
  ELSE
     PQMONPROC.InsLog(nIdProceso,'M','Se inicia Generacion de Outgoing Visa-NAIGUATA: ' || vFileOutVi || ' a las ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
      dbms_output.put_line('**** INICIA  GENERACION DE OUTGOING VISA-NGTA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
  END IF;

  file_id1 := utl_file.FOPEN( kDirOUT_Alias2, vFileOutVi1, 'W' );

  OPEN Cr_EX9001;
     dbms_output.put_line('**** ABRIENDO CURSOR EX9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     PQMONPROC.InsLog(nIdProceso,'M','Abriendo Cursor EX9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     -- Se carga primero los registros Rechazados
     p_CargaRegRechazados_ventas(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
     BEGIN

     FETCH Cr_EX9001 INTO  rCr_EX9001;
      EXIT WHEN Cr_EX9001%NOTFOUND;
           IF rCr_EX9001.CODPRO_P03_ACTC IN ('000000','010000','200000') AND
              (rCr_EX9001.IDEMEN_P00_ACTC = '1240' OR (rCr_EX9001.IDEMEN_P00_ACTC = '1440' AND rCr_EX9001.REFADQ_P31_ACTC is not null)) THEN
            /*  IF SUBSTR(rCr_EX8001.PUNSER_P22_ACTC, 7, 1) IS NOT NULL THEN
                 cont := cont+1;
              END IF;    */
              vTCR_0.Trn_Code := f_Trn_Code(rCr_EX9001.IDEMEN_P00_ACTC,rCr_EX9001.CODPRO_P03_ACTC, rCr_EX9001.ORIIDE_P56_ACTC);
                  dbms_output.put_line('vTCR_0.Trn_Code:'||vTCR_0.Trn_Code);
              vTCR_0.Trn_Code_Qlf := '0';
              vTCR_0.Trn_Comp_Seq_Num := '0';
              vTCR_0.Account_Num := rpad(SUBSTR(rCr_EX9001.NUMTAR_P02_ACTC,1,19),19,'0');
                                                dbms_output.put_line('vTCR_0.Account_Num:'||vTCR_0.Account_Num);
              vTCR_0.Floor_Limit_Ind:=' ';
              vTCR_0.CRB_Excep_File_Ind := ' ';
              vTCR_0.PCAS_ind := ' ';
              vTCR_0.Acq_Ref_Num := rCr_EX9001.REFADQ_P31_ACTC;
                                  dbms_output.put_line('vTCR_0.Acq_Ref_Num:'||vTCR_0.Acq_Ref_Num);
              vTCR_0.Acq_Bus_ID := LPAD('0',8,'0'); --'00000000';
              IF (rCr_EX9001.ORITIM_P56_ACTC IS NULL) OR (rCr_EX9001.ORITIM_P56_ACTC = '000000000000') THEN
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX9001.TIMLOC_P12_ACTC,3,4);
              ELSE
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX9001.ORITIM_P56_ACTC,3,4);
              END IF;
              vTCR_0.Dest_Amount := LPAD('0',12,'0'); --'000000000000';
              vTCR_0.Dest_Curren_Code := LPAD(' ',3,' '); --'   ';
              vTCR_0.Source_Amount :=  f_Source_Amount(rCr_EX9001.impcon_p05_actc ,rCr_EX9001.imptra_p04_actc);
              vTCR_0.Source_Curren_Code := f_Source_Currency_Code(rCr_EX9001.impcon_p05_actc ,rCr_EX9001.moncon_p50_actc ,rCr_EX9001.montra_p49_actc);
              IF ltrim(rtrim(rCr_EX9001.CODACT_P26_ACTC)) >= '3000' AND ltrim(rtrim(rCr_EX9001.CODACT_P26_ACTC)) <= '3299' THEN
                 vTCR_0.Merch_Name := f_buscap26(rCr_EX9001.CODACT_P26_ACTC) || '             ';
              ELSE
                 vTCR_0.Merch_Name := RPAD(ltrim(rtrim(rCr_EX9001.NOMEST_P43_ACTC)),25,' ');
              END IF;
              vTCR_0.Merch_city := NVL(RPAD(ltrim(rtrim(rCr_EX9001.LOCEST_P43_ACTC)),13,' '),'             ');
               vTCR_0.Merch_Count_Code := RPAD(ltrim(rtrim(rCr_EX9001.PAIEST_P43_ACTC)),3,' ');
              vTCR_0.Merch_Cate_Code  := NVL(ltrim(rtrim(rCr_EX9001.CODACT_P26_ACTC)),'    ');
              vTCR_0.Merch_Zip_Code   := LPAD('0',5,'0'); --'00000';
              vTCR_0.Merch_Prov_Code  := LPAD(' ',3,' '); --'   ';
              vTCR_0.Req_Payment_Serv :=' ';
              vTCR_0.Reserved_01 := ' ' ;
              vTCR_0.Usage_Code := f_Usage_Code_05(rCr_EX9001.codfun_p24_actc);
              vTCR_0.Reason_Code := f_Reason_Code(rCr_EX9001.CODRAZ_P25_ACTC,rCr_EX9001.codfun_p24_actc);
              vTCR_0.Settlement_Flag  := f_settlement_flag(rCr_EX9001.BINADQ_P48_ACTC, rCr_EX9001.INLOTE_P29_ACTC);
                vTCR_0.Aut_Charact_Ind := ' '; --f_Aut_Charact_Ind(vTCR_0.Trn_Code, SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,5,1) ,SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,6,1));
              IF rCr_EX9001.NUMAUT_P38_ACTC is not null THEN
                 vTCR_0.Authorization_Code := rpad(LTRIM(RTRIM(rCr_EX9001.NUMAUT_P38_ACTC)),6,' ');
              ELSE
                 vTCR_0.Authorization_Code := lpad(' ',6,' ');
              END IF;
              vTCR_0.POS_Capability  := f_Pos_Terminal_Capability(SUBSTR(rCr_EX9001.PUNSER_P22_ACTC,1,1));
               vTCR_0.Cardholder_ID_Met := f_Cardholder_IDMethod(SUBSTR(rCr_EX9001.PUNSER_P22_ACTC,8,1));
              vTCR_0.Collec_Only_Flag :=' ';
              vTCR_0.POS_Entry_Mode := f_POS_EntryMode(SUBSTR(rCr_EX9001.PUNSER_P22_ACTC,7,1),SUBSTR(rCr_EX9001.PUNSER_P22_ACTC,5,1));
              vTCR_0.Inter_Fee_Ind := f_international_fee_indicator(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode);
              IF vTCR_0.Trn_Code = '25' THEN
                 vTCR_0.Central_Proc_Date := f_JulianDate(SUBSTR(rCr_EX9001.TIMLOC_P12_ACTC,1,6));
              ELSE
                 vTCR_0.Central_Proc_Date := '0000'; --f_JulianDate(SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,1,6));
              END IF;
              vTCR_0.Reimb_Attribute := f_reimbursement_attribute(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode, rCr_EX9001.INLOTE_P29_ACTC, vTCR_0.Trn_Code);

              linea := LPAD(vTCR_0.Trn_Code,2,'0')|| vTCR_0.Trn_Code_Qlf || vTCR_0.Trn_Comp_Seq_Num ||
                       vTCR_0.Account_Num || vTCR_0.Floor_Limit_Ind ||vTCR_0.CRB_Excep_File_Ind  ||
                       vTCR_0.PCAS_ind || vTCR_0.Acq_Ref_Num ||lpad(vTCR_0.Acq_Bus_ID,8,'0') ||
                       vTCR_0.Purchase_date || LPAD(vTCR_0.Dest_Amount,12,'0') ||vTCR_0.Dest_Curren_Code ||
                       LPAD(to_char(vTCR_0.Source_Amount),12,'0') || vTCR_0.Source_Curren_Code ||
                       vTCR_0.Merch_Name || vTCR_0.Merch_city || vTCR_0.Merch_Count_Code ||
                       vTCR_0.Merch_Cate_Code || vTCR_0.Merch_Zip_Code || vTCR_0.Merch_Prov_Code ||
                       vTCR_0.Req_Payment_Serv || vTCR_0.Reserved_01 || vTCR_0.Usage_Code ||
                       lpad(vTCR_0.Reason_Code,2,'0') || vTCR_0.Settlement_Flag || vTCR_0.Aut_Charact_Ind ||
                       vTCR_0.Authorization_Code || vTCR_0.POS_Capability || vTCR_0.Inter_Fee_Ind ||
                       vTCR_0.Cardholder_ID_Met || vTCR_0.Collec_Only_Flag || vTCR_0.POS_Entry_Mode ||
                       vTCR_0.Central_Proc_Date || vTCR_0.Reimb_Attribute;
                    dbms_output.put_line('lineea1:'||linea);
              pqOutput.PUT_LINE(linea);
          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              p_Update_Contadores(SUBSTR(vTCR_0.Acq_Ref_Num,2,6), vtcr_0.Trn_Code, vtcr_0.Usage_Code, vtcr_0.Source_Curren_Code, vtcr_0.Settlement_Flag, vtcr_0.Source_Amount, rCr_EX9001.IMPCU1_P46_ACTC);
          --  Contadores para TC1
              vNum_Tcrs_tc91:= vNum_Tcrs_tc91+1;
              vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_0.Source_Amount;
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;
              vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_0.Source_Amount;

  -------------------------------------------------TCR1----------------------------------

              vCtaSelec := ' ';
              IF SUBSTR(rCr_EX9001.FILLER_P48_ACTC, 6, 3) = vCodMoneda THEN
                 vCtaSelec := '0';
              END IF;
              IF SUBSTR(rCr_EX9001.FILLER_P48_ACTC, 6, 3) = '840' THEN
                 vCtaSelec := '1';
              END IF;

              vTCR_1.Trn_Code := f_Trn_Code(rCr_EX9001.IDEMEN_P00_ACTC,rCr_EX9001.CODPRO_P03_ACTC, rCr_EX9001.ORIIDE_P56_ACTC);
              vTCR_1.Trn_Code_Qlf :='0';
              vTCR_1.Trn_Comp_Seq_Num :='1';
              vTCR_1.Iss_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Acq_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Chargeback_Ref_Num := f_Chrgback_Ref_Num(rCr_EX9001.CODFUN_P24_ACTC,rCr_EX9001.NUMEMI_P95_ACTC);
              --vTCR_1.Documentation_Ind := f_Documentation_Indicator(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.CODRAZ_P25_ACTC);
              p_Documentation_indicator(rCr_EX9001.CODFUN_P24_ACTC,rCr_EX9001.refadq_p31_actc, nIdProceso, vTCR_1.Documentation_Ind);
              vTCR_1.Member_Mess_Text := f_member_message_text(rCr_EX9001.CODFUN_P24_ACTC, rCr_EX9001.NUMEMI_P95_ACTC, rCr_EX9001.LONR50_P48_ACTC, rCr_EX9001.CODR50_P48_ACTC, rCr_EX9001.VTAPLA_P48_ACTC, rCr_EX9001.DIFERI_P48_ACTC);
              vTCR_1.Special_Cond_Ind := LPAD(' ',2,' '); --'  '
              vTCR_1.FeeProgramID    := LPAD(' ',3,' '); --'  '
              vTCR_1.Issuer_charge := LPAD(' ',1,' ');
              vTCR_1.Reserved_01 := LPAD(' ',1,' '); --'     '
              vTCR_1.Card_Acceptor_ID := rpad(rCr_EX9001.IDEEST_P42_ACTC, 15, ' ');--rpad((substr(rCr_EX8001.IDEEST_P42_ACTC,3,3) || substr(rCr_EX8001.IDEEST_P42_ACTC,10,6)) || vCtaSelec, 15, ' ');
              vTCR_1.Terminal_ID := f_Terminal_ID(rCr_EX9001.PUNSER_P22_ACTC, rCr_EX9001.IDETER_P41_ACTC, rCr_EX9001.REFADQ_P31_ACTC, rCr_EX9001.IDEADQ_P32_ACTC);
              --vTCR_1.Terminal_ID := rCr_EX8001.IDETER_P41_ACTC;--Cambio pedido de AV 06 SEP 2004
              vTCR_1.National_Reimb_Fee := f_Nat_Reimbursement_fee_06(VTCR_0.Settlement_Flag,rCr_EX9001.IMPCU1_P46_ACTC);
              vTCR_1.Mail_Phone_Ind := f_mail_telephon_indicator_06(rCr_EX9001.PUNSER_P22_ACTC,rCr_EX9001.CODACT_P26_ACTC);
              vTCR_1.Spec_Chargeback_Ind := ' ';
              vTCR_1.Interface_Trace_Num := LPAD('0',6,'0'); --'000000';
              --vTCR_1.Cardhold_ActTer_Ind := ' '; --comentado pedido AV
              vTCR_1.Cardhold_ActTer_Ind := f_Cardhold_ActTer_Ind(rCr_EX9001.IDETER_P41_ACTC, rCr_EX9001.IDEADQ_P32_ACTC, rCr_EX9001.TIPMOV_P48_ACTC);
              vTCR_1.Prepaid_Card_Ind := ' ';
              vTCR_1.Reserved_02 := ' ';
              vTCR_1.AVS_Response_Code := ' ';
              vTCR_1.Aut_Source_Code := ' ';
              vTCR_1.Purchase_Iden_Form := ' ';
              vTCR_1.ATM_Account_Select := '0';
              vTCR_1.Inst_Payment_Count := '  ';
              vTCR_1.Purchase_Identifier := LPAD(' ',25,' '); --'                         ';
              varCashback := NVL(LPAD(rCr_EX9001.MCASHB_P48_ACTC, 9, '0'), '000000000');
              vTCR_1.Chip_Cond_Code := f_Chip_Cond_Code(rCr_EX9001.PUNSER_P22_ACTC);
              vTCR_1.POS_Environment  := f_POS_Environment(rCr_EX9001.PUNSER_P22_ACTC);

              linea := LPAD(vTCR_1.Trn_Code,2,'0') ||   LPAD(vTCR_1.Trn_Code_Qlf,1,'0') ||  vTCR_1.Trn_Comp_Seq_Num ||
                       vTCR_1.Iss_Workstat_BIN || vTCR_1.Acq_Workstat_BIN ||   lpad(vTCR_1.Chargeback_Ref_Num,6,'0') ||
                       vTCR_1.Documentation_Ind || rpad(vTCR_1.Member_Mess_Text,50,' ') ||   vTCR_1.Special_Cond_Ind ||
                       vTCR_1.FeeProgramID || vTCR_1.Issuer_charge ||
                       vTCR_1.Reserved_01 ||  vTCR_1.Card_Acceptor_ID ||   rpad(vTCR_1.Terminal_ID,8,' ') ||
                       lpad(vTCR_1.National_Reimb_Fee,12,'0') ||  vTCR_1.Mail_Phone_Ind ||   vTCR_1.Spec_Chargeback_Ind ||
                       lpad(vTCR_1.Interface_Trace_Num,6,'0') ||  vTCR_1.Cardhold_ActTer_Ind ||   vTCR_1.Prepaid_Card_Ind ||
                       vTCR_1.Reserved_02 ||  vTCR_1.AVS_Response_Code ||   vTCR_1.Aut_Source_Code ||
                       vTCR_1.Purchase_Iden_Form || vTCR_1.ATM_Account_Select ||  vTCR_1.Inst_Payment_Count ||
                       vTCR_1.Purchase_Identifier || varCashback ||  vTCR_1.Chip_Cond_Code ||  vTCR_1.POS_Environment;
                    dbms_output.put_line('lineea2:'||linea);
              pqOutput.PUT_LINE(linea);

          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  Contadores para tc91 y 92 del Tc07
              vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's (Se agrega)
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's (Se agrega)

-----------------
--vTCR_5 y vTCR_7
-----------------

              IF SUBSTR(rCr_EX9001.PUNSER_P22_ACTC,7,1)='5' THEN

                 SELECT COUNT(1)
                   INTO VCUENTA
                   FROM CLR_DCEMVFULL
                  WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX9001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                    AND IDETRA_P11_ACTC = rCr_EX9001.IDETRA_P11_ACTC
                    AND TIMLOC_P12_ACTC = rCr_EX9001.TIMLOC_P12_ACTC
                    AND IDEADQ_P32_ACTC = rCr_EX9001.IDEADQ_P32_ACTC;

                 IF VCUENTA > 0  THEN

                    SELECT TTCRIP_P55_ACTC,NUMSEC_P23_ACTC,FECCRI_P55_ACTC,CAPTER_P55_ACTC,
                           PAICRI_P55_ACTC,NUMALE_P55_ACTC,EMVATC_P55_ACTC,PERINT_P55_ACTC,
                           CRIPPE_P55_ACTC,DAPLEM_P55_ACTC,EMVTVR_P55_ACTC,IMPCRI_P55_ACTC,
                           SCREMI_P55_ACTC, RESCRI_P55_ACTC
                      INTO VTTCRIP_P55_ACTC,VNUMSEC_P23_ACTC,VFECCRI_P55_ACTC,VCAPTER_P55_ACTC,
                           VPAICRI_P55_ACTC,VNUMALE_P55_ACTC,VEMVATC_P55_ACTC,VPERINT_P55_ACTC,
                           VCRIPPE_P55_ACTC,VDAPLEM_P55_ACTC,VEMVTVR_P55_ACTC,VIMPCRI_P55_ACTC,
                           VSCREMI_P55_ACTC,VRESCRI_P55_ACTC
                      FROM CLR_DCEMVFULL
                     WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX9001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                       AND IDETRA_P11_ACTC = rCr_EX9001.IDETRA_P11_ACTC
                       AND TIMLOC_P12_ACTC = rCr_EX9001.TIMLOC_P12_ACTC
                       AND IDEADQ_P32_ACTC = rCr_EX9001.IDEADQ_P32_ACTC;

                    vTCR_5.Trn_Code                       := f_Trn_Code(rCr_EX9001.IDEMEN_P00_ACTC,rCr_EX9001.CODPRO_P03_ACTC,rCr_EX9001.ORIIDE_P56_ACTC);       --   1-  2   Transaction Code --LPAD('0',2,'0');
                    vTCR_5.Trn_Code_Qlf                   := '0';
                    vTCR_5.Trn_Comp_Seq_Num               := '5';
                    vTCR_5.TransactionIdentifier          := NVL(substr(rCr_EX9001.TransactionIdentifier,1,15), LPAD('0',15,'0')) ; --TID
                    --dbms_output.put_line('ENTRO 1 : ' || vTCR_5.TransactionIdentifier);
                    vTCR_5.AuthorizedAmount               := LPAD(TO_CHAR(rCr_EX9001.IMPTRA_P04_ACTC),12,'0');  --LPAD('0',12,'0');
                    vTCR_5.AuthorizationCurrencyCode      := LPAD(rCr_EX9001.MONTRA_P49_ACTC,3,' ');  --pSource_Curren_Code; --LPAD(' ',3,' ');
                    dbms_output.put_line('vTCR_5.AuthorizationCurrencyCode:'||vTCR_5.AuthorizationCurrencyCode);
                    vTCR_5.AuthorizationResponseCode      := LPAD(NVL(TRIM(VRESCRI_P55_ACTC),0),2,'0'); --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0');
                    vTCR_5.ValidationCode                 := LPAD(' ',4,' ');
                    vTCR_5.ExcludedTranIdenReason         := LPAD(' ',1,' ');
                    vTCR_5.CRSProcessingCode              := LPAD(' ',1,' ');
                    vTCR_5.ChargebackRightsIndicator      := LPAD(' ',2,' ');
                    vTCR_5.MultipleClearingSequenceNumber := LPAD('0',2,'0');
                    vTCR_5.MultipleClearingSequenceCount  := LPAD('0',2,'0');
                    vTCR_5.MarketSpecificAutDataInd       := LPAD(' ',1,' ');
                    vTCR_5.TotalAuthorizedAmount          := LPAD('0',12,'0');
                    vTCR_5.InformationIndicator           := LPAD(' ',1,' ');
                    vTCR_5.MerchantTelephoneNumber        := LPAD(' ',14,' ');
                    vTCR_5.AdditionalDataIndicator        := LPAD(' ',1,' ');
                    vTCR_5.MerchantVolumeIndicator        := LPAD(' ',2,' ');
                    vTCR_5.ElectronicCommGoodsInd         := LPAD(' ',2,' ');
                    vTCR_5.MerchantVerificationValue      := LPAD(' ',10,' ');
                    vTCR_5.InterchangeFeeAmount           := LPAD('0',15,'0');
                    vTCR_5.InterchangeFeeSign             := LPAD(' ',1,' ');
                    vTCR_5.SourceCurrBaseCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.BaseCurrtoDestCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.OptionalIssuerISAAmount        := LPAD('0',12,'0');
                    vTCR_5.ProductID                      := LPAD(' ',2,' ');
                    vTCR_5.ProgramID                      := LPAD(' ',6,' ');
                    vTCR_5.Reserved                       := LPAD(' ',24,' ');
                    vTCR_5.CVV2ResultCode                 := LPAD(' ',1,' ');


                     IF TRIM(vTCR_5.ProductID) = 'I' THEN
                        vTCR_5.ProductID := 'I1';
                    END IF;

                    linea := LPAD(vTCR_5.Trn_Code,2,'0')||vTCR_5.Trn_Code_Qlf||
                             vTCR_5.Trn_Comp_Seq_Num||vTCR_5.TransactionIdentifier||
                             vTCR_5.AuthorizedAmount||vTCR_5.AuthorizationCurrencyCode||
                             vTCR_5.AuthorizationResponseCode||vTCR_5.ValidationCode||
                             vTCR_5.ExcludedTranIdenReason||vTCR_5.CRSProcessingCode||
                             vTCR_5.ChargebackRightsIndicator||vTCR_5.MultipleClearingSequenceNumber||
                             vTCR_5.MultipleClearingSequenceCount||vTCR_5.MarketSpecificAutDataInd||
                             vTCR_5.TotalAuthorizedAmount||vTCR_5.InformationIndicator||
                             vTCR_5.MerchantTelephoneNumber||vTCR_5.AdditionalDataIndicator||
                             vTCR_5.MerchantVolumeIndicator||vTCR_5.ElectronicCommGoodsInd||
                             vTCR_5.MerchantVerificationValue||vTCR_5.InterchangeFeeAmount||
                             vTCR_5.InterchangeFeeSign||vTCR_5.SourceCurrBaseCurrExchRate||
                             vTCR_5.BaseCurrtoDestCurrExchRate||vTCR_5.OptionalIssuerISAAmount||
                             vTCR_5.ProductID||vTCR_5.ProgramID||
                             vTCR_5.Reserved||
                             vTCR_5.CVV2ResultCode;



                    --dbms_output.put_line('ENTRO 1 linea det:' || linea);

                    pqOutput.PUT_LINE(linea);
                    --Contadores para tc91 y 92 del Tc05
                    vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;
                    vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;


                    vTCR_7.Trn_Code := f_Trn_Code(rCr_EX9001.IDEMEN_P00_ACTC,rCr_EX9001.CODPRO_P03_ACTC,rCr_EX9001.ORIIDE_P56_ACTC);
                    vTCR_7.Trn_Code_Qlf := '0';
                    vTCR_7.Trn_Comp_Seq_Num := '7';
                    vTCR_7.dctcttcrip := LPAD(NVL(VTTCRIP_P55_ACTC,'0'),2,'0');
                    vTCR_7.dcnumsec := LPAD(NVL(VNUMSEC_P23_ACTC,0),3,'0');
                    vTCR_7.dctcfeccri := LPAD(VFECCRI_P55_ACTC,6,'0');
                    vTCR_7.dctccapter := LPAD(VCAPTER_P55_ACTC,6,'0');
                    vTCR_7.dctcpaicri := '862'; -- RPAD(VPAICRI_P55_ACTC,3,' ');
                    vTCR_7.Num_serie := LPAD (' ',8,' '); --LPAD(rCr_EX8001.IDETER_P41_ACTC,8,' '); --' ';
                    vTCR_7.dctcnumale := LPAD(VNUMALE_P55_ACTC,8,'0');
                    vTCR_7.dctcemvatc := LPAD(VEMVATC_P55_ACTC,4,'0');
                    vTCR_7.dctcperint := LPAD(VPERINT_P55_ACTC,4,'0');
                    vTCR_7.dctccrippe := LPAD(VCRIPPE_P55_ACTC,16,'0');
                    vTCR_7.dctcdaplem01 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),3,2),2,'0');
                    vTCR_7.dctcdaplem02 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),5,2),2,'0');
                    vTCR_7.dctcemvtvr := LPAD(VEMVTVR_P55_ACTC,10,'0');
                    vTCR_7.dctcdaplem03 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),7,8),8,'0');
                    vTCR_7.dctcimpcri := LPAD(VIMPCRI_P55_ACTC,12,'0');
                    vTCR_7.dctcdaplem04 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),15,2),2,'0');
                    vTCR_7.dctcdaplem05 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),17,16),16,'0');
                    vTCR_7.dctcdaplem06 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),1,2),2,'0');
                    vTCR_7.dctcdaplem07 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),33,2),2,'0');
                    vTCR_7.dctcdaplem08 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),35,30),30,'0');
                    vTCR_7.Espacios := lpad(' ',8,' ');
                    vTCR_7.dctcscremi := nvl(LPAD(STD.F_RTRIM4HEX(VSCREMI_P55_ACTC),10,'0'),kSCREMI_P55_ACTC_DEF);

                    linea := LPAD(vTCR_7.Trn_Code,2,'0') || vTCR_7.Trn_Code_Qlf || vTCR_7.Trn_Comp_Seq_Num ||
                             vTCR_7.dctcttcrip || LPAD(vTCR_7.dcnumsec,3,'0')|| vTCR_7.dctcfeccri||vTCR_7.dctccapter||
                             vTCR_7.dctcpaicri||vTCR_7.Num_serie||vTCR_7.dctcnumale||vTCR_7.dctcemvatc||
                             vTCR_7.dctcperint||vTCR_7.dctccrippe||vTCR_7.dctcdaplem01||vTCR_7.dctcdaplem02||
                             vTCR_7.dctcemvtvr||vTCR_7.dctcdaplem03||vTCR_7.dctcimpcri||vTCR_7.dctcdaplem04||
                             vTCR_7.dctcdaplem05||vTCR_7.dctcdaplem06||vTCR_7.dctcdaplem07||vTCR_7.dctcdaplem08||
                             vTCR_7.Espacios||vTCR_7.dctcscremi;

                    dbms_output.put_line('linea:' || linea);
                    pqOutput.PUT_LINE(linea);

                     --  Contadores para tc91 y 92 del Tc07
                     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
                     vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's
                 ELSE
                    IF vTCR_0.Trn_Code = '05' then
                       dbms_output.put_line('Alerta TCR_0 CHIP NO CARGADA:  ' || rCr_EX9001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                       PQMONPROC.InsLog(nIdProceso,'W','Alerta TCR_0 CHIP NO CARGADA: ' || rCr_EX9001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                    END IF;
                 END IF;


              END IF;

          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
          --  **********Para el TC91************
              vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
              vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
              vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
              vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
              IF vNum_MTrans_tc91 = 500 THEN
                 vNum_Tcrs_tc91 := vNum_Tcrs_tc91+1;--Numero de TCR's..91
                 vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's..92
                 vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                 vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                 vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                 vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                 linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                          vBatch_Number_tc91,vNum_Tcrs_tc91,
                          vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                 pqOutput.PUT_LINE(linea);
                 vNum_MTrans_tc91 := 0;
                 vNum_Tcrs_tc91 := 0;
                 vNum_Trns_tc91 := 0;
                 vSource_Amount_tc91 := 0;
              END IF;
           END IF;
  EXCEPTION
     WHEN OTHERS THEN
          vSQLError:=SQLCODE;
          vSqlErrM :=SUBSTR(SQLERRM,1,200);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR EX9001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.InsLog(nIdProceso, 'E', 'INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA','99');
            dbms_output.put_line('******************************************************************************************');
             dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
             dbms_output.put_line('**** PROCESANDO CURSOR EX9001 - ' || vDesc_EntAdq );
             dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA');
             dbms_output.put_line('******************************************************************************************');
          RETURN;
     END;
  END LOOP;
  dbms_output.put_line('**** CERRANDO CURSOR EX9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor EX9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));

  CLOSE Cr_EX9001;

  -- Para el TC91

  IF vNum_Tcrs_tc91 >= 0 THEN
 -- las sgtes 4 lineas han sido descomentadas EL 04 MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91, vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     pqOutput.PUT_LINE(linea);
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:=0;
  END If;
-- Fin para TC91

  OPEN Cr_CLR_COPAE9001;
  dbms_output.put_line('**** ABRIENDO CURSOR COPAE9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       PQMONPROC.inslog(nIdProceso,'M','Abriendo Cursor COPAE9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       p_CargaRegRechazados(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
   BEGIN
      FETCH Cr_CLR_COPAE9001 INTO  rCr_COPAE9001;
      EXIT WHEN Cr_CLR_COPAE9001%NOTFOUND;
         IF rCr_COPAE9001.IDEMEN_P00_ACCP = '1740'  THEN
            vTCR_10_20.Trn_Code := f_tc10_20_Trn_Code(rCr_COPAE9001.SIGCU1_P46_ACCP);
            vTCR_10_20.Trn_Code_Qlf:= '0';
            vTCR_10_20.Trn_Comp_Seq_Num:= '0';
            vTCR_10_20.Destination_BIN := '000000';
            IF SUBSTR(rCr_COPAE9001.IDEADQ_P32_ACCP,3,4) ='0105' THEN
               vTCR_10_20.Source_BIN := '411032';
            ELSIF SUBSTR(rCr_COPAE9001.IDEADQ_P32_ACCP,3,4) ='0108' THEN
               vTCR_10_20.Source_BIN := '420198';
            END IF;
            vTCR_10_20.Reason_Code := f_tc10_Reason_Code(rCr_COPAE9001.CODRAZ_P25_ACCP);
            vTCR_10_20.Country_Code := f_Country_Code(vTCR_10_20.Reason_Code);
            vTCR_10_20.Event_Date:= SUBSTR(rCr_COPAE9001.TIMLOC_P12_ACCP,3,4);
            vTCR_10_20.Account_Num := rpad(ltrim(rtrim(rCr_COPAE9001.numtar_p02_accp)),19,'0');
            IF (ltrim(rtrim(rCr_COPAE9001.numtar_p02_accp)) = '' OR TO_NUMBER(rCr_COPAE9001.numtar_p02_accp)=0 OR rCr_COPAE9001.numtar_p02_accp is null) AND
               (rCr_COPAE9001.codraz_p25_accp = '7703') THEN
               IF substr(rCr_COPAE9001.regdat_p72_accp,12,4) ='0002' THEN
                    vTCR_10_20.Destination_BIN := '450645';
                ELSE
                   IF substr(rCr_COPAE9001.regdat_p72_accp,12,4) ='0003' THEN
                      vTCR_10_20.Destination_BIN := '454775';
                  ELSE
                     IF substr(rCr_COPAE9001.regdat_p72_accp,12,4) ='0011' THEN
                         vTCR_10_20.Destination_BIN := '492112';
                     END IF;
                  END IF;
               END IF;
            END IF;
            vTCR_10_20.Dest_Amount:= 0;
            vTCR_10_20.Dest_Curren_Code := '   ';
            IF rCr_COPAE9001.IMPCU1_P46_ACCP is null THEN
               vTCR_10_20.Source_Amount := 0;
            ELSE
               vTCR_10_20.Source_Amount := rCr_COPAE9001.IMPCU1_P46_ACCP;
            END IF;
            vTCR_10_20.Source_Curren_Code:= vCodMoneda;
            vTCR_10_20.Mess_Text:= RPAD(rCr_COPAE9001.TEXTO_P104_ACCP,70,' ');
            vTCR_10_20.Settlement_Flag := f_tc10_Settlement_flag(rCr_COPAE9001.INLOTE_P29_ACCP);--vTCR_10_20.Source_BIN, rCr_COPAE8001.TIPCU1_P46_ACCP);
            IF SUBSTR(rCr_COPAE9001.numtar_p02_accp,1,16) = '4506450000000016' and rCr_COPAE9001.codraz_p25_accp='7711' AND substr(rCr_COPAE9001.texto_p104_accp,1,14) = 'SECURITIZACION' THEN
               vTCR_10_20.Settlement_Flag := '0';
            END IF;
            vTCR_10_20.Trans_Identifier:=   rCr_EX9001.TransactionIdentifier ; -- 0;
          --  dbms_output.put_line('ENTRO 2 vTCR_10_20.Trans_Identifier : ' || vTCR_10_20.Trans_Identifier);
            vTCR_10_20.Reserved_01:= ' ';
            vTCR_10_20.Central_Proc_Date:= '0000';
            vTCR_10_20.Reimb_Attribute:='0';
            linea := vTCR_10_20.Trn_Code ||  vTCR_10_20.Trn_Code_Qlf ||   vTCR_10_20.Trn_Comp_Seq_Num||
                     lpad(vTCR_10_20.Destination_BIN,6,'0') || lpad(vTCR_10_20.Source_BIN,6,'0') ||   lpad(vTCR_10_20.Reason_Code,4,'0') ||
                     vTCR_10_20.Country_Code || lpad(vTCR_10_20.Event_Date,4,'0') ||   vTCR_10_20.Account_Num ||
                     lpad(vTCR_10_20.Dest_Amount,12,'0') || vTCR_10_20.Dest_Curren_Code ||
                     lpad(vTCR_10_20.Source_Amount,12,'0') || vTCR_10_20.Source_Curren_Code || vTCR_10_20.Mess_Text ||
                     vTCR_10_20.Settlement_Flag || LPAD(vTCR_10_20.Trans_Identifier,15,'0') ||  vTCR_10_20.Reserved_01 ||
                     vTCR_10_20.Central_Proc_Date || vTCR_10_20.Reimb_Attribute;
             --dbms_output.put_line('ENTRO 2 det: '|| linea);
             pqOutput.PUT_LINE(linea);
             utl_file.PUT_LINE( file_id1, linea);
             p_Update_Contadores(vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, '1', vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.Source_Amount, 0);
             p_Inserta_10_20(vFechac, vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, rCr_COPAE9001.CODRAZ_P25_ACCP, vTCR_10_20.Source_Amount);
             --**********Para el TC91************
             vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_10_20.Source_Amount;
             vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_10_20.Source_Amount;
             vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
             vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
             vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
             vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's
             vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
             vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
             IF vNum_MTrans_tc91 = 500 THEN
                vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
                vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
                vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                         vBatch_Number_tc91,vNum_Tcrs_tc91,
                         vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                pqOutput.PUT_LINE(linea);
                vNum_MTrans_tc91 := 0;
                vNum_Tcrs_tc91 :=0;
                vNum_Trns_tc91 := 0;
                vSource_Amount_tc91:= 0;
             END IF;
          END IF;

   EXCEPTION
      WHEN OTHERS THEN
           vSQLError:=SQLCODE;
           vSqlErrM :=SUBSTR(SQLERRM,1,200);
           PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE9001 - ' || vDesc_EntAdq);
           vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
           PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
              dbms_output.put_line('******************************************************************************************');
              dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
              dbms_output.put_line('**** PROCESANDO CURSOR COPAE9001 - Banco: ' || vDesc_EntAdq);
              dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ');
              dbms_output.put_line('******************************************************************************************');
           RETURN;
   END;

  END LOOP;
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor COPAE9001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  CLOSE Cr_CLR_COPAE9001;
  -- Para el TC91
  IF vNum_Tcrs_tc91 >=0 THEN
     -- LAS SGTES 4 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                            vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:= 0;
     pqOutput.PUT_LINE(linea);
  END IF;
   -- Para el TC92
   -- LAS SGTES 2 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
  vNum_Tcrs_tc92 := vNum_Tcrs_tc92 + 1;
  vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
  vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
  linea := f_gen_TCR_9X('92', vNum_MTrans_tc92 ,
                         vBatch_Number_tc92,vNum_Tcrs_tc92,
                         vNum_Trns_tc92,vSource_Amount_tc92,vFechac6) ;

  pqOutput.PUT_LINE(linea);
   dbms_output.put_line('**** -----------------------------------------------------------------------');
   dbms_output.put_line('**** TOME NOTA DE ESTE NUMERO PARA EL CUADRE CONTRA EL CLEARING ');
   dbms_output.put_line('**** OUTGOING DE VISA : ' || lpad(to_char(vNum_Trns_tc92) || ' TRANSACCIONES',52, ' '));
   dbms_output.put_line('**** -----------------------------------------------------------------------');

  vNum_MTrans_tc92 := 0;
  vNum_Tcrs_tc92 :=0;
  vNum_Trns_tc92 := 0;
  vSource_Amount_tc92:=0;
  vBatch_Number_tc92:=0;

  -- Fin Para el TC92
  -- FIN DE CLR_COPAE9001************************************/
  pqOutput.Close_file;--SE CERRO EL ARCHIVO OUTVIEEEEYYYYMMDD.DAT
  utl_file.fCLOSE(file_id1);-- SE CERRO EL ARCHIVO OUTVIEEEEYYMMDD.TXT
  dbms_output.put_line('****************************************************************************');
  dbms_output.put_line('**** TERMINA GENERACION DE OUTGOING VISA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
  dbms_output.put_line('****************************************************************************');

  PQMONPROC.inslog(nIdProceso,'M','Termina Generacion de Outgoing Visa Naiguata: ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
  vMensaje := pqmonproc.updmonproc (nIdProceso, 'F');
  --FIN DE ARCHIVO OUTVIEEEEYYYYMMDD.DAT ********
 EXCEPTION
     WHEN r_error Then
     dbms_output.put_line('vSqlErrM: ' || vSqlErrM || ' '|| ' pIdProceso:' || nIdProceso);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE9001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
          RETURN;
 END p_OutGoingVisa_NGTA;

--<< FIN P_OUTGOINGVISA >>-------------------------------------------------------------------
---------------------------------------------------------------------

 FUNCTION f_Aut_Charact_Ind(Trans_Code IN VARCHAR2, puns05_p22 IN VARCHAR2,puns06_p22 IN VARCHAR2) RETURN CHAR
 IS
/******************************************************
    FUNCION: f_Aut_Charact_Ind
    ARGUMENTOS: Trans_Code NUMBER, puns05_p22
                puns06_p22
    RETURN:   f_Aut_Charact_Ind
    AUTOR: JCF
    FECHA: 05.11.03
    DESCRIPCION: Esta funcion devuelve el Authorization Characteristics Indicator.
******************************************************/
 BEGIN
    IF Trans_Code = '05' Or Trans_Code = '06' THEN
       IF puns06_p22 = '0' Then
          IF puns05_p22 = '7' Or puns05_p22 = '8' THEN
             RETURN  'N';
          ELSE
             RETURN 'M';
          END IF;
       ELSE
          RETURN 'A';
       END IF;
    ELSE
       IF puns06_p22 = '0' THEN
          RETURN 'N';
       ELSE
          RETURN 'E';
       END IF;
    END IF;
 END;

---------------------------------------------------------------------

 FUNCTION f_Pos_Terminal_Capability(puns01_22_actc IN VARCHAR2) RETURN CHAR
 /******************************************************
    FUNCION: f_Pos_Terminal_Capability
    ARGUMENTOS: puns01_22_actc VARCHAR2
    RETURN: puns01_22_actc
    AUTOR: JCF
    FECHA: 06.11.03
    DESCRIPCION: Devuelve el puns01_22_actc o 1.
******************************************************/
 IS
 BEGIN
    IF puns01_22_actc = '2' THEN
       RETURN '2';
    ELSE
       IF puns01_22_actc = '5' THEN
          RETURN '5';
       ELSE
          RETURN '1';
       END IF;
    END IF;
 END f_Pos_Terminal_Capability;

---------------------------------------------------------------------

 FUNCTION f_International_Fee_Indicator(Sflag IN VARCHAR, PosEntryMode IN VARCHAR) RETURN CHAR
 IS
 BEGIN
    --   International_Fee_Indicator := ' ';
       RETURN ' '; --Agregado GlobaR 29-09-2015  IPR 1185
    /*  Comentado GlobaR 29-09-2015  IPR 1185
    IF Sflag = '0' And PosEntryMode = '90' THEN
       RETURN '2';
    ELSE
       RETURN ' ';
    END IF;*/
 END f_International_Fee_Indicator;

---------------------------------------------------------------------

 FUNCTION f_Settlement_flag(w_binadq IN VARCHAR2, INLOTE_P29_ACTC IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF INLOTE_P29_ACTC = '512' THEN
       RETURN '8';
    ELSIF INLOTE_P29_ACTC = '514' THEN
       RETURN '0';
    END IF;
 END f_Settlement_flag;

---------------------------------------------------------------------

 FUNCTION f_POS_EntryMode(puns07p22actc IN VARCHAR2,puns05p22actc IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF puns07p22actc = '1' THEN
       IF puns05p22actc = '3' THEN
          RETURN '01';
       ELSE
          RETURN '00';
       END IF;
    ELSIF puns07p22actc = '2' THEN
       RETURN'90';
    ELSIF puns07p22actc = '5' THEN
       RETURN '05';
    ELSIF puns07p22actc = '6' THEN
       RETURN '01';
    ELSE
       RETURN '00';
    END IF;
 END f_POS_EntryMode;

---------------------------------------------------------------------

 FUNCTION f_Cardholder_IDMethod(puns08p22actc IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF puns08p22actc = '0' THEN
       RETURN ' ';
    ELSE
       IF puns08p22actc = '1' THEN
          RETURN '2';
       ELSE
          IF puns08p22actc = '5' THEN
             RETURN '1';
          ELSE
             RETURN' ';
          END IF;
       END IF;
    END IF;
 END f_Cardholder_IDMethod;

---------------------------------------------------------------------

 FUNCTION f_Reimbursement_attribute(Sflag IN VARCHAR2, PosEntryMode IN VARCHAR2, INLOTE_P29_ACTC IN VARCHAR2, transCode IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF --Sflag = '0' And PosEntryMode in ('90','05') And INLOTE_P29_ACTC = '514' And Not (
       transCode in ( '05','15','25','35','06','26') THEN
       RETURN 'B';
    ELSE
       RETURN '0';
    END IF;
 END f_Reimbursement_attribute;

---------------------------------------------------------------------

 FUNCTION f_JulianDate(p12 IN VARCHAR2 ) RETURN CHAR
 IS
 date_julian varchar2(6);
 BEGIN
    date_julian:= to_char(to_date(p12,'YYMMDD'),'YDDD');
    RETURN date_julian;
 END f_JulianDate;

---------------------------------------------------------------------

 FUNCTION f_Usage_Code_05(codfun_r_p24_actc IN VARCHAR2)RETURN CHAR
 IS
 BEGIN
    IF codfun_r_p24_actc='205' or codfun_r_p24_actc='206' THEN
       RETURN '2';
       --Evaluar Reason_Code
    ELSE
       RETURN '1';
    END IF;
 END f_Usage_Code_05;

---------------------------------------------------------------------

 FUNCTION F_Trn_Code(IDEMEN_P00_ACTC IN VARCHAR2,
                    CODPRO_P03_ACTC IN VARCHAR2,
                    ORIIDE_P56_ACTC IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF IDEMEN_P00_ACTC='1240' AND CODPRO_P03_ACTC IN ('000000','010000') THEN
       RETURN '05';
    ELSE
       IF IDEMEN_P00_ACTC='1240' AND CODPRO_P03_ACTC IN ('200000') THEN
          RETURN '06';
       ELSE
          IF (IDEMEN_P00_ACTC='1440' AND ORIIDE_P56_ACTC = '1240') AND CODPRO_P03_ACTC IN ('000000','010000') THEN
             RETURN '25';
          END IF;
       END IF;
    END IF;
 END F_Trn_Code;
---------------------------------------------------------------------

 FUNCTION F_Trn_Code_rev(IDEMEN_P00_ACTC IN VARCHAR2,
                    CODPRO_P03_ACTC IN VARCHAR2,
                    ORIIDE_P56_ACTC IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF IDEMEN_P00_ACTC='1240' AND CODPRO_P03_ACTC IN ('000000','010000') THEN
       RETURN '25';
    ELSE
       IF IDEMEN_P00_ACTC='1240' AND CODPRO_P03_ACTC IN ('200000') THEN
          RETURN '26';
       ELSE
          IF (IDEMEN_P00_ACTC='1440' AND ORIIDE_P56_ACTC = '1240') AND CODPRO_P03_ACTC IN ('000000','010000') THEN
             RETURN '05';
          END IF;
       END IF;
    END IF;
 END F_Trn_Code_rev;

---------------------------------------------------------------------

 FUNCTION f_Reason_Code(CODRAZ_P25_ACTC IN VARCHAR2,
                       codfun_r_p24_actc IN VARCHAR2) RETURN VARCHAR2
 IS
 Cod_raz    NUMBER;
 BEGIN
    IF codfun_r_p24_actc='205' OR codfun_r_p24_actc='206' THEN
       SELECT c9CodVisa INTO Cod_raz
         FROM CODIGOS_DE_RAZON_DEL_MENSAJE
        WHERE p25CodRaz = CODRAZ_P25_ACTC;
       RETURN NVL(LPAD(Cod_raz, 2, '0'), '00');
    ELSE
       RETURN '00';
    END IF;
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
      -- NO HAY DATA
         RETURN '00';
    WHEN TOO_MANY_ROWS THEN
      -- MAS DE 1 REG RECUPERADO
         RETURN '00';
    WHEN OTHERS THEN
         RETURN '00';
 END f_Reason_Code;

---------------------------------------------------------------------

 FUNCTION f_Source_Amount(impcon_p05_actc NUMBER,
                         imptra_p04_actc NUMBER)RETURN NUMBER
 IS
 BEGIN
    IF impcon_p05_actc IS NULL THEN
       RETURN imptra_p04_actc;
    ELSE
       IF impcon_p05_actc > 0 THEN
          RETURN impcon_p05_actc;
       ELSE
          RETURN imptra_p04_actc;
       END IF;
    END IF;
 END f_Source_Amount;

---------------------------------------------------------------------

 FUNCTION f_Source_Currency_Code(impcon_p05_actc NUMBER,
                                moncon_p50_actc varchar2,
                                montra_p49_actc varchar2 )RETURN CHAR
 IS
 BEGIN
    IF impcon_p05_actc IS NULL THEN
       RETURN montra_p49_actc;
    ELSE
       IF impcon_p05_actc > 0 THEN
          RETURN moncon_p50_actc;
       ELSE
          RETURN montra_p49_actc;
       END IF;
    END IF;
 END f_Source_Currency_Code;

---------------------------------------------------------------------

 FUNCTION f_Nat_Reimbursement_fee_06(sFlag IN VARCHAR2,
                                    imcutr_r_p46_actc_1 IN VARCHAR2) RETURN NUMBER
 IS
 BEGIN
    IF sFlag = 8 THEN
       RETURN imcutr_r_p46_actc_1;
    ELSE
       RETURN 0;
    END IF;
 END f_Nat_Reimbursement_fee_06;

---------------------------------------------------------------------

 FUNCTION f_mail_telephon_indicator_06(punser_P22_actc IN VARCHAR2,
                                      codact_p26_actc IN VARCHAR2) RETURN CHAR
 IS
 mail_tel_ind  VARCHAR2(1):=' ' ;
 BEGIN
    --* Cambios VISANET por p22
    IF substr(punser_P22_actc,5,1) ='8' THEN -- motivo CES avv060899
       IF substr(punser_P22_actc,8,1) = '8' THEN
          mail_tel_ind := '7';
       ELSE
          IF substr(punser_P22_actc,8,1) = '0' THEN
             mail_tel_ind :=  '8';
          ELSE
             mail_tel_ind :=  ' ';
          END IF;
       END IF;
    ELSE
       IF substr(punser_P22_actc,5,1) ='7' THEN
          IF substr(punser_P22_actc,8,1) = '8' THEN
             mail_tel_ind :=  '5';
          ELSE
             IF substr(punser_P22_actc,8,1) = '0' THEN
                mail_tel_ind :=  '6';
             ELSE
                mail_tel_ind :=  ' ';
             END IF;
          END IF;
       ELSE
          IF substr(punser_P22_actc,5,1) = '3' THEN
             -- mail_tel_ind :=  '2'; -- pedido AV 07 SEP 2003
             mail_tel_ind := ' ';
          ELSE
             IF substr(punser_P22_actc,5,1) = '2' THEN
                mail_tel_ind :=  '1';
             ELSE
                mail_tel_ind :=  ' ';
             END IF; --* If substr(punser_P22_actc,5,1) = '2'
          END IF; --* If substr(punser_P22_actc,5,1) = '3'
       END IF; --* If substr(punser_P22_actc,5,1) ='7'
    END IF; --* If substr(punser_P22_actc,5,1) ='8'
    IF codact_p26_actc in('5960', '5962', '5964','5965', '5966', '5967', '5968', '5969') AND (mail_tel_ind = ' ') Then
       RETURN '1';
    ELSE
       RETURN mail_tel_ind;
    END IF;
 END f_mail_telephon_indicator_06;

---------------------------------------------------------------------

-- FUNCTION f_Chip_Cond_Code(PUNSER_P22_ACTC IN VARCHAR2, cont IN NUMBER) RETURN CHAR
 FUNCTION f_Chip_Cond_Code(PUNSER_P22_ACTC IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF substr(PUNSER_P22_ACTC,7,1) IN ('S','T') THEN
       RETURN '1';
    ELSE
       RETURN ' ';
    END IF;
   /* IF cont = 1 THEN
       RETURN ' ';
    ELSE
       IF SUBSTR(PUNSER_P22_ACTC, 7, 1) = '5' THEN
          RETURN '1';
       ELSIF SUBSTR(PUNSER_P22_ACTC, 7, 1) = 'S' THEN
          RETURN '2';
       ELSE
          RETURN ' ';
       END IF;
    END IF;*/
 END f_Chip_Cond_Code;

---------------------------------------------------------------------

 FUNCTION f_Chrgback_Ref_Num(codfun_p24_actc IN VARCHAR,
                            numemi_p95_actc IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF codfun_p24_actc IN ('205','206') THEN
       RETURN NVL(LPAD(RTRIM(LTRIM(substr(numemi_p95_actc,1,6))), 6, '0'), '000000');
    ELSE
       RETURN '000000';
    END IF;
 END f_Chrgback_Ref_Num;

---------------------------------------------------------------------

--FUNCTION f_Documentation_indicator( /*codfun_p24_actc IN VARCHAR,
 PROCEDURE p_Documentation_indicator (
                                   p24 VARCHAR2,
                                   p31 VARCHAR2,
                                   nIdProceso NUMBER,
                                   Pinc_doc_ind out varchar2) --RETURN CHAR
 IS
 BEGIN
    IF p24 IN ('205','206') THEN
       WITH tabla_01 AS (
          SELECT NVL(trim(incoming_documentation_ind),' ') incoming_documentation_ind
            FROM disputas_master
           WHERE incoming_refadq = p31
             AND disputa_stat    = 'T'
             AND (disputa_type   = '15' OR disputa_type    = '16')
          ORDER BY disputa_id DESC)

          SELECT incoming_documentation_ind INTO Pinc_doc_ind
            FROM tabla_01
           WHERE ROWNUM < 2;
    ELSE
       Pinc_doc_ind := ' ';
    END IF;
 EXCEPTION
    WHEN OTHERS THEN
         pqmonproc.inslog(nIdProceso, 'M', 'No se encontro la transaccion');
 END p_Documentation_indicator;
--END f_Documentation_indicator;

---------------------------------------------------------------------

 FUNCTION f_member_message_text (codfun_r_p24_actc IN VARCHAR2,
                                numemi_p95_actc IN VARCHAR2,
                                lonr50_p48_actc IN VARCHAR2,
                                codr50_p48_actc IN VARCHAR2,
                                vtapla_p48_actc IN VARCHAR2,
                                vdiferi_p48_actc VARCHAR2)RETURN CHAR
 IS
 v_member_message_text VARCHAR2(50);
 varTxt                CHAR(50);
 BEGIN
    v_member_message_text :='';
    varTxt := '';
    IF codfun_r_p24_actc IN ('205', '206') THEN -- Representacion
       varTxt := NVL(RPAD(RTRIM(LTRIM(SUBSTR(numemi_p95_actc, 7, 50))), 50, ' '), RPAD(' ', 50, ' '));
    ELSE
       IF lonr50_p48_actc = '004' AND codr50_p48_actc = '50' THEN
          v_member_message_text := RPAD('PLAZO' || vtapla_p48_actc, 50, ' ');
          IF vdiferi_p48_actc IS NOT NULL THEN
             IF vdiferi_p48_actc <> '0' THEN
                varTxt := NVL(RPAD(RTRIM(LTRIM(v_member_message_text)) || vdiferi_p48_actc,50,' '), RPAD(' ', 50, ' '));
             ELSE
                varTxt := v_member_message_text;
             END IF;
          ELSE
             varTxt := v_member_message_text;
          END IF;
       ELSE
          varTxt := RPAD(' ', 50, ' ');
       END IF;
    END IF;
    RETURN varTxt;
 END f_member_message_text;

---------------------------------------------------------------------

 FUNCTION f_CashBack(MCASHB_P48_ACTC IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF MCASHB_P48_ACTC IS NULL THEN
       RETURN '000000000';
    ELSE
       RETURN LPAD(MCASHB_P48_ACTC,7,'0');
    END IF;
 END;

---------------------------------------------------------------------

 FUNCTION f_tc10_20_Trn_Code(SIGCU1_P46_ACCP  IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF SIGCU1_P46_ACCP = 'D' THEN
        ----> TC 10
       RETURN '10';
    ELSE
        ----> TC 20
       RETURN '20';
    END IF;
 END f_tc10_20_Trn_Code;
---------------------------------------------------------------------

 FUNCTION f_tc10_20_Trn_Code_rev(SIGCU1_P46_ACCP  IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF SIGCU1_P46_ACCP = 'D' THEN
        ----> TC 20
       RETURN '20';
    ELSE
        ----> TC 10
       RETURN '10';
    END IF;
 END f_tc10_20_Trn_Code_rev;
---------------------------------------------------------------------

 FUNCTION f_tc10_Settlement_flag(INLOTE_P29_ACCP IN VARCHAR2) RETURN VARCHAR2
 IS
 BEGIN
    IF INLOTE_P29_ACCP = '512' THEN
       RETURN '8';
    ELSIF INLOTE_P29_ACCP = '514' THEN
       RETURN '0';
    END IF;
 END f_tc10_Settlement_flag;

---------------------------------------------------------------------

 FUNCTION f_Country_Code(Reason_Code IN VARCHAR2) RETURN CHAR
 IS
 BEGIN
    IF Reason_Code = 100 OR Reason_Code = 190 THEN
       RETURN 'PE ';
    ELSE
       RETURN '   ';
    END IF;
 END f_Country_Code;

---------------------------------------------------------------------

 FUNCTION f_tc10_Reason_Code(CODRAZ_P25_ACCP IN VARCHAR2) RETURN VARCHAR2
 IS
 Cod_raz    NUMBER;
 BEGIN
    SELECT c9CodVisa into Cod_raz
      FROM CODIGOS_DE_RAZON_DEL_MENSAJE
     WHERE p25CodRaz = CODRAZ_P25_ACCP;
    RETURN NVL(LPAD(Cod_raz, 4, '0'), '0000');
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
         RETURN '0000';
    WHEN TOO_MANY_ROWS THEN
         RETURN '0000';
    WHEN OTHERS THEN
         RETURN '0000';
 END f_tc10_Reason_Code;

---------------------------------------------------------------------

 FUNCTION f_gen_TCR_9X(vTc                  VARCHAR2,
                      vNum_MTrans_tc9X     NUMBER,
                      vBatch_Number_tc9X   NUMBER,
                      vNum_Tcrs_tc9X       NUMBER,
                      vNum_Trns_tc9X       NUMBER,
                      vSource_Amount_tc9X  NUMBER,
                      vProcess_Date        VARCHAR2) RETURN VARCHAR2
 IS
 linea           varchar2(168);
 vTCR_9X         TCR_9X;
 BEGIN
    vTCR_9X.Trn_Code := vTc;
    vTCR_9X.Trn_Code_Qlf := 0;
    vTCR_9X.Trn_Comp_Seq_Num := 0;
    vTCR_9X.BIN:=LPAD('0',6,'0'); --'000000';
    vTCR_9X.Process_Date:= TO_CHAR(TO_DATE(vProcess_Date,'YYMMDD'),'YY')||TO_CHAR(TO_DATE(vProcess_Date,'YYMMDD'),'DDD');
    vTCR_9X.Dest_Amount:=0;
    vTCR_9X.Num_Money_Trns:= vNum_MTrans_tc9X;
    vTCR_9X.Batch_Num:= vBatch_Number_tc9X;
    vTCR_9X.Num_TCRs:= vNum_Tcrs_tc9X;
    vTCR_9X.Reserved_01:=LPAD('0',6,'0'); --'000000';
    vTCR_9X.Center_Batch_ID:=LPAD(' ',8,' '); --'        ';
    vTCR_9X.Num_Trns:= vNum_Trns_tc9X;
    vTCR_9X.Reserved_02:=LPAD('0',18,'0'); --'000000000000000000';
    vTCR_9X.Source_Amount:= vSource_Amount_tc9X;
    vTCR_9X.Reserved_03:=LPAD('0',15,'0'); --'000000000000000';
    vTCR_9X.Reserved_04:=LPAD('0',15,'0'); --'000000000000000';
    vTCR_9X.Reserved_05:=LPAD('0',15,'0'); --'000000000000000';
    vTCR_9X.Reserved_06:=LPAD('0',7,'0'); --'       ';

    linea:= vTCR_9X.Trn_Code || vTCR_9X.Trn_Code_Qlf ||
            vTCR_9X.Trn_Comp_Seq_Num || vTCR_9X.BIN ||
            vTCR_9X.Process_Date || lpad(vTCR_9X.Dest_Amount,15,'0') ||
            lpad(vTCR_9X.Num_Money_Trns,12,'0') || lpad(vTCR_9X.Batch_Num,6,'0') ||
            lpad(vTCR_9X.Num_TCRs,12,'0') || vTCR_9X.Reserved_01 ||
            rpad(vTCR_9X.Center_Batch_ID,8,' ') || lpad(vTCR_9X.Num_Trns,9,'0') ||
            vTCR_9X.Reserved_02 || lpad(vTCR_9X.Source_Amount,15,'0') ||
            vTCR_9X.Reserved_03 || vTCR_9X.Reserved_04 ||
            vTCR_9X.Reserved_05 || vTCR_9X.Reserved_06;
    RETURN linea;
 END f_gen_TCR_9X;

---------------------------------------------------------------------

 PROCEDURE p_Inicializa_Contadores(vFechaProceso CHAR,
                                  vMoneda varchar2)
 IS
 BEGIN
-- Inicializando Acumuladores para el BIN 491955
    tabla_491955:=T_bins();
    tabla_491955.extend;
    tabla_491955(1).Trn_Code := '05';
    tabla_491955(1).Usage_Code := '1';
    tabla_491955(1).Source_Curren_Code := vMoneda;
    tabla_491955(1).Settlement_Flag := '0';
    tabla_491955(1).Total_Source_Amount := 0;
    tabla_491955(1).Total_Trans := 0;
    tabla_491955(1).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(2).Trn_Code:='05';
    tabla_491955(2).Usage_Code:='1';
    tabla_491955(2).Source_Curren_Code:= vMoneda;
    tabla_491955(2).Settlement_Flag:='8';
    tabla_491955(2).Total_Source_Amount := 0;
    tabla_491955(2).Total_Trans := 0;
    tabla_491955(2).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(3).Trn_Code:='05';
    tabla_491955(3).Usage_Code:='2';
    tabla_491955(3).Source_Curren_Code:= vMoneda ;
    tabla_491955(3).Settlement_Flag:='0';
    tabla_491955(3).Total_Source_Amount := 0;
    tabla_491955(3).Total_Trans := 0;
    tabla_491955(3).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(4).Trn_Code:='05';
    tabla_491955(4).Usage_Code:='2';
    tabla_491955(4).Source_Curren_Code:= vMoneda ;
    tabla_491955(4).Settlement_Flag:='8';
    tabla_491955(4).Total_Source_Amount := 0;
    tabla_491955(4).Total_Trans := 0;
    tabla_491955(4).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(5).Trn_Code:='06';
    tabla_491955(5).Usage_Code:='1';
    tabla_491955(5).Source_Curren_Code:= vMoneda;
    tabla_491955(5).Settlement_Flag:='0';
    tabla_491955(5).Total_Source_Amount := 0;
    tabla_491955(5).Total_Trans := 0;
    tabla_491955(5).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(6).Trn_Code:='06';
    tabla_491955(6).Usage_Code:='1';
    tabla_491955(6).Source_Curren_Code:= vMoneda;
    tabla_491955(6).Settlement_Flag:='8';
    tabla_491955(6).Total_Source_Amount := 0;
    tabla_491955(6).Total_Trans := 0;
    tabla_491955(6).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(7).Trn_Code:='25';
    tabla_491955(7).Usage_Code:='1';
    tabla_491955(7).Source_Curren_Code:= vMoneda;
    tabla_491955(7).Settlement_Flag:='0';
    tabla_491955(7).Total_Source_Amount := 0;
    tabla_491955(7).Total_Trans := 0;
    tabla_491955(7).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(8).Trn_Code:='25';
    tabla_491955(8).Usage_Code:='1';
    tabla_491955(8).Source_Curren_Code:= vMoneda;
    tabla_491955(8).Settlement_Flag:='8';
    tabla_491955(8).Total_Source_Amount := 0;
    tabla_491955(8).Total_Trans := 0;
    tabla_491955(8).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(9).Trn_Code:='10';
    tabla_491955(9).Usage_Code:='1';
    tabla_491955(9).Source_Curren_Code:= vMoneda;
    tabla_491955(9).Settlement_Flag:='0';
    tabla_491955(9).Total_Source_Amount := 0;
    tabla_491955(9).Total_Trans := 0;
    tabla_491955(9).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(10).Trn_Code:='10';
    tabla_491955(10).Usage_Code:='1';
    tabla_491955(10).Source_Curren_Code:= vMoneda;
    tabla_491955(10).Settlement_Flag:='8';
    tabla_491955(10).Total_Source_Amount := 0;
    tabla_491955(10).Total_Trans := 0;
    tabla_491955(10).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(11).Trn_Code:='20';
    tabla_491955(11).Usage_Code:='1';
    tabla_491955(11).Source_Curren_Code:= vMoneda;
    tabla_491955(11).Settlement_Flag:='0';
    tabla_491955(11).Total_Source_Amount := 0;
    tabla_491955(11).Total_Trans := 0;
    tabla_491955(11).Total_Comisiones := 0;
    tabla_491955.extend;
    tabla_491955(12).Trn_Code:='20';
    tabla_491955(12).Usage_Code:='1';
    tabla_491955(12).Source_Curren_Code:= vMoneda;
    tabla_491955(12).Settlement_Flag:='8';
    tabla_491955(12).Total_Source_Amount := 0;
    tabla_491955(12).Total_Trans := 0;
    tabla_491955(12).Total_Comisiones := 0;
-- Inicializando Acumuladores para el BIN 423632
    tabla_423632:=T_bins();
    tabla_423632.extend;
    tabla_423632(1).Trn_Code := '05';
    tabla_423632(1).Usage_Code := '1';
    tabla_423632(1).Source_Curren_Code := '840';
    tabla_423632(1).Settlement_Flag := '0';
    tabla_423632(1).Total_Source_Amount := 0;
    tabla_423632(1).Total_Trans := 0;
    tabla_423632(1).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(2).Trn_Code:='05';
    tabla_423632(2).Usage_Code:='1';
    tabla_423632(2).Source_Curren_Code:='840';
    tabla_423632(2).Settlement_Flag:='8';
    tabla_423632(2).Total_Source_Amount := 0;
    tabla_423632(2).Total_Trans := 0;
    tabla_423632(2).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(3).Trn_Code:='05';
    tabla_423632(3).Usage_Code:='2';
    tabla_423632(3).Source_Curren_Code:='840';
    tabla_423632(3).Settlement_Flag:='0';
    tabla_423632(3).Total_Source_Amount := 0;
    tabla_423632(3).Total_Trans := 0;
    tabla_423632(3).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(4).Trn_Code:='05';
    tabla_423632(4).Usage_Code:='2';
    tabla_423632(4).Source_Curren_Code:='840';
    tabla_423632(4).Settlement_Flag:='8';
    tabla_423632(4).Total_Source_Amount := 0;
    tabla_423632(4).Total_Trans := 0;
    tabla_423632(4).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(5).Trn_Code:='06';
    tabla_423632(5).Usage_Code:='1';
    tabla_423632(5).Source_Curren_Code:='840';
    tabla_423632(5).Settlement_Flag:='0';
    tabla_423632(5).Total_Source_Amount := 0;
    tabla_423632(5).Total_Trans := 0;
    tabla_423632(5).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(6).Trn_Code:='06';
    tabla_423632(6).Usage_Code:='1';
    tabla_423632(6).Source_Curren_Code:='840';
    tabla_423632(6).Settlement_Flag:='8';
    tabla_423632(6).Total_Source_Amount := 0;
    tabla_423632(6).Total_Trans := 0;
    tabla_423632(6).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(7).Trn_Code:='25';
    tabla_423632(7).Usage_Code:='1';
    tabla_423632(7).Source_Curren_Code:='840';
    tabla_423632(7).Settlement_Flag:='0';
    tabla_423632(7).Total_Source_Amount := 0;
    tabla_423632(7).Total_Trans := 0;
    tabla_423632(7).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(8).Trn_Code:='25';
    tabla_423632(8).Usage_Code:='1';
    tabla_423632(8).Source_Curren_Code:='840';
    tabla_423632(8).Settlement_Flag:='8';
    tabla_423632(8).Total_Source_Amount := 0;
    tabla_423632(8).Total_Trans := 0;
    tabla_423632(8).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(9).Trn_Code:='10';
    tabla_423632(9).Usage_Code:='1';
    tabla_423632(9).Source_Curren_Code:='840';
    tabla_423632(9).Settlement_Flag:='0';
    tabla_423632(9).Total_Source_Amount := 0;
    tabla_423632(9).Total_Trans := 0;
    tabla_423632(9).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(10).Trn_Code:='10';
    tabla_423632(10).Usage_Code:='1';
    tabla_423632(10).Source_Curren_Code:='840';
    tabla_423632(10).Settlement_Flag:='8';
    tabla_423632(10).Total_Source_Amount := 0;
    tabla_423632(10).Total_Trans := 0;
    tabla_423632(10).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(11).Trn_Code:='20';
    tabla_423632(11).Usage_Code:='1';
    tabla_423632(11).Source_Curren_Code:='840';
    tabla_423632(11).Settlement_Flag:='0';
    tabla_423632(11).Total_Source_Amount := 0;
    tabla_423632(11).Total_Trans := 0;
    tabla_423632(11).Total_Comisiones := 0;
    tabla_423632.extend;
    tabla_423632(12).Trn_Code:='20';
    tabla_423632(12).Usage_Code:='1';
    tabla_423632(12).Source_Curren_Code:='840';
    tabla_423632(12).Settlement_Flag:='8';
    tabla_423632(12).Total_Source_Amount := 0;
    tabla_423632(12).Total_Trans := 0;
    tabla_423632(12).Total_Comisiones := 0;
-- Inicializando Acumuladores del BIN 423617
    tabla_423617:=T_bins();
    tabla_423617.extend;
    tabla_423617(1).Trn_Code := '05';
    tabla_423617(1).Usage_Code := '1';
    tabla_423617(1).Source_Curren_Code := vMoneda;
    tabla_423617(1).Settlement_Flag := '0';
    tabla_423617(1).Total_Source_Amount := 0;
    tabla_423617(1).Total_Trans := 0;
    tabla_423617(1).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(2).Trn_Code:='05';
    tabla_423617(2).Usage_Code:='1';
    tabla_423617(2).Source_Curren_Code:= vMoneda;
    tabla_423617(2).Settlement_Flag:='8';
    tabla_423617(2).Total_Source_Amount := 0;
    tabla_423617(2).Total_Trans := 0;
    tabla_423617(2).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(3).Trn_Code:='05';
    tabla_423617(3).Usage_Code:='2';
    tabla_423617(3).Source_Curren_Code:= vMoneda;
    tabla_423617(3).Settlement_Flag:='0';
    tabla_423617(3).Total_Source_Amount := 0;
    tabla_423617(3).Total_Trans := 0;
    tabla_423617(3).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(4).Trn_Code:='05';
    tabla_423617(4).Usage_Code:='2';
    tabla_423617(4).Source_Curren_Code:= vMoneda;
    tabla_423617(4).Settlement_Flag:='8';
    tabla_423617(4).Total_Source_Amount := 0;
    tabla_423617(4).Total_Trans := 0;
    tabla_423617(4).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(5).Trn_Code:='06';
    tabla_423617(5).Usage_Code:='1';
    tabla_423617(5).Source_Curren_Code:= vMoneda;
    tabla_423617(5).Settlement_Flag:='0';
    tabla_423617(5).Total_Source_Amount := 0;
    tabla_423617(5).Total_Trans := 0;
    tabla_423617(5).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(6).Trn_Code:='06';
    tabla_423617(6).Usage_Code:='1';
    tabla_423617(6).Source_Curren_Code:= vMoneda;
    tabla_423617(6).Settlement_Flag:='8';
    tabla_423617(6).Total_Source_Amount := 0;
    tabla_423617(6).Total_Trans := 0;
    tabla_423617(6).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(7).Trn_Code:='25';
    tabla_423617(7).Usage_Code:='1';
    tabla_423617(7).Source_Curren_Code:= vMoneda;
    tabla_423617(7).Settlement_Flag:='0';
    tabla_423617(7).Total_Source_Amount := 0;
    tabla_423617(7).Total_Trans := 0;
    tabla_423617(7).Total_Comisiones := 0;
    tabla_423617.extend;
    tabla_423617(8).Trn_Code:='25';
    tabla_423617(8).Usage_Code:='1';
    tabla_423617(8).Source_Curren_Code:= vMoneda;
    tabla_423617(8).Settlement_Flag:='8';
    tabla_423617(8).Total_Source_Amount := 0;
    tabla_423617(8).Total_Trans := 0;
    tabla_423617(8).Total_Comisiones := 0;
    DELETE FROM TC10_TC20
    WHERE Fecha_Proceso = vFechaProceso;
    COMMIT;
 END p_Inicializa_Contadores;

---------------------------------------------------------------------

 PROCEDURE p_Update_Contadores(vBin VARCHAR,
                              vTc VARCHAR,
                              vUsage_Code VARCHAR,
                              vMoneda VARCHAR,
                              vSettlementFlag VARCHAR,
                              vTotal_Source_Amount NUMBER,
                              vTotal_Comisiones NUMBER)
 IS
 BEGIN
    --CodMoneda es una variable global, se obtiene de la funcion PQCOMERCIOS.GCW_F_GETMONEDAVIG(fec date)
    IF vBin = '491955' AND vMoneda = vCodMoneda THEN
       --************ Para el Tc 05 **************************--
       IF vTC = '05' THEN
          IF vUsage_Code = '1' THEN
             IF vSettlementFlag = '0' THEN
                tabla_491955(1).Total_Source_Amount:=tabla_491955(1).Total_Source_Amount + vTotal_Source_Amount;
                tabla_491955(1).Total_Trans:= tabla_491955(1).Total_Trans + 1;
                tabla_491955(1).Total_Comisiones:= tabla_491955(1).Total_Comisiones + vTotal_Comisiones;
                RETURN;
             END IF;
             IF vSettlementFlag = '8' THEN
                tabla_491955(2).Total_Source_Amount:=tabla_491955(2).Total_Source_Amount + vTotal_Source_Amount;
                tabla_491955(2).Total_Trans:= tabla_491955(2).Total_Trans + 1;
                tabla_491955(2).Total_Comisiones:= tabla_491955(2).Total_Comisiones + vTotal_Comisiones;
                RETURN;
             END IF;
          ELSIF vUsage_Code = '2' THEN
             IF vSettlementFlag = '0' THEN
                tabla_491955(3).Total_Source_Amount:=tabla_491955(3).Total_Source_Amount + vTotal_Source_Amount;
                tabla_491955(3).Total_Trans:= tabla_491955(3).Total_Trans + 1;
                tabla_491955(3).Total_Comisiones:= tabla_491955(3).Total_Comisiones + vTotal_Comisiones;
                RETURN;
             END IF;
             IF vSettlementFlag = '8' THEN
                tabla_491955(4).Total_Source_Amount:=tabla_491955(4).Total_Source_Amount + vTotal_Source_Amount;
                tabla_491955(4).Total_Trans:= tabla_491955(4).Total_Trans + 1;
                tabla_491955(4).Total_Comisiones:= tabla_491955(4).Total_Comisiones + vTotal_Comisiones;
             END IF;
          END IF; -- Usage Code = 2 and Moneda = 604
       END IF; -- TC 05
   --************ Para el Tc 06 **************************--
       IF vTc = '06' and vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_491955(5).Total_Source_Amount:=tabla_491955(5).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(5).Total_Trans:= tabla_491955(5).Total_Trans + 1;
             tabla_491955(5).Total_Comisiones:= tabla_491955(5).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_491955(6).Total_Source_Amount:=tabla_491955(6).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(6).Total_Trans:= tabla_491955(6).Total_Trans + 1;
            tabla_491955(6).Total_Comisiones:= tabla_491955(6).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 06
   --************ Para el Tc 25 **************************--
       IF vTc = '25' and vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_491955(7).Total_Source_Amount:=tabla_491955(7).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(7).Total_Trans:= tabla_491955(7).Total_Trans + 1;
             tabla_491955(7).Total_Comisiones:= tabla_491955(7).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_491955(8).Total_Source_Amount:=tabla_491955(8).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(8).Total_Trans:= tabla_491955(8).Total_Trans + 1;
             tabla_491955(8).Total_Comisiones:= tabla_491955(8).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 25
--************ Para el Tc 10 **************************--
       IF vTC = '10' THEN
          IF vSettlementFlag = '0' THEN
             tabla_491955(9).Total_Source_Amount := tabla_491955(9).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(9).Total_Trans := tabla_491955(9).Total_Trans + 1;
             tabla_491955(9).Total_Comisiones := tabla_491955(9).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_491955(10).Total_Source_Amount:=tabla_491955(10).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(10).Total_Trans:= tabla_491955(10).Total_Trans + 1;
             tabla_491955(10).Total_Comisiones:= tabla_491955(10).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 10
--************ Para el Tc 20 **************************--
       IF vTc = '20' Then
          IF vSettlementFlag = '0' THEN
             tabla_491955(11).Total_Source_Amount:=tabla_491955(11).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(11).Total_Trans:= tabla_491955(11).Total_Trans + 1;
             tabla_491955(11).Total_Comisiones:= tabla_491955(11).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_491955(12).Total_Source_Amount:=tabla_491955(12).Total_Source_Amount + vTotal_Source_Amount;
             tabla_491955(12).Total_Trans:= tabla_491955(12).Total_Trans + 1;
             tabla_491955(12).Total_Comisiones:= tabla_491955(12).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 20
    END IF; -- If BIN = 491955
    IF vBin = '423632' and vMoneda = '840' THEN
       IF vTC = '05' THEN
   --************ Para el Tc 05 **************************--
          IF vUsage_Code = '1' THEN
             IF vSettlementFlag = '0' THEN
                tabla_423632(1).Total_Source_Amount:=tabla_423632(1).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423632(1).Total_Trans:= tabla_423632(1).Total_Trans + 1;
                tabla_423632(1).Total_Comisiones:= tabla_423632(1).Total_Comisiones + vTotal_Comisiones;
             ELSIF vSettlementFlag = '8' THEN
                tabla_423632(2).Total_Source_Amount:=tabla_423632(2).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423632(2).Total_Trans:= tabla_423632(2).Total_Trans + 1;
                tabla_423632(2).Total_Comisiones:= tabla_423632(2).Total_Comisiones + vTotal_Comisiones;
             END IF;
          ELSIF vUsage_Code = '2' THEN
             IF vSettlementFlag = '0' THEN
                tabla_423632(3).Total_Source_Amount:=tabla_423632(3).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423632(3).Total_Trans:= tabla_423632(3).Total_Trans + 1;
                tabla_423632(3).Total_Comisiones:= tabla_423632(3).Total_Comisiones + vTotal_Comisiones;
             ELSIF vSettlementFlag = '8' THEN
                tabla_423632(4).Total_Source_Amount:=tabla_423632(4).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423632(4).Total_Trans:= tabla_423632(4).Total_Trans + 1;
                tabla_423632(4).Total_Comisiones:= tabla_423632(4).Total_Comisiones + vTotal_Comisiones;
             END IF;
          END IF; -- Usage Code = 2 and Moneda = 840
       END IF; -- TC 05
       --************ Para el Tc 06 **************************--
       IF vTc = '06' and vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423632(5).Total_Source_Amount:=tabla_423632(5).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(5).Total_Trans:= tabla_423632(5).Total_Trans + 1;
             tabla_423632(5).Total_Comisiones:= tabla_423632(5).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423632(6).Total_Source_Amount:=tabla_423632(6).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(6).Total_Trans:= tabla_423632(6).Total_Trans + 1;
             tabla_423632(6).Total_Comisiones:= tabla_423632(6).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 06
   --************ Para el Tc 25 **************************--
       IF vTc = '25' and vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423632(7).Total_Source_Amount:=tabla_423632(7).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(7).Total_Trans:= tabla_423632(7).Total_Trans + 1;
             tabla_423632(7).Total_Comisiones:= tabla_423632(7).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423632(8).Total_Source_Amount:=tabla_423632(8).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(8).Total_Trans:= tabla_423632(8).Total_Trans + 1;
             tabla_423632(8).Total_Comisiones:= tabla_423632(8).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 25
--************ Para el Tc 10 **************************--
       IF vTC = '10' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423632(9).Total_Source_Amount:=tabla_423632(9).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(9).Total_Trans:= tabla_423632(9).Total_Trans + 1;
             tabla_423632(9).Total_Comisiones:= tabla_423632(9).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423632(10).Total_Source_Amount:=tabla_423632(10).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(10).Total_Trans:= tabla_423632(10).Total_Trans + 1;
             tabla_423632(10).Total_Comisiones:= tabla_423632(10).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 10
--************ Para el Tc 20 **************************--
       IF vTc = '20' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423632(11).Total_Source_Amount:=tabla_423632(11).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(11).Total_Trans:= tabla_423632(11).Total_Trans + 1;
             tabla_423632(11).Total_Comisiones:= tabla_423632(11).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423632(12).Total_Source_Amount:=tabla_423632(12).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423632(12).Total_Trans:= tabla_423632(12).Total_Trans + 1;
             tabla_423632(12).Total_Comisiones:= tabla_423632(12).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 20
    END IF; -- If BIN = 423632
    IF vBin = '423617' and vMoneda = vCodMoneda THEN
       IF vTC = '05' THEN
   --************ Para el Tc 05 **************************--
          IF vUsage_Code = '1' THEN
             IF vSettlementFlag = '0' THEN
                tabla_423617(1).Total_Source_Amount:=tabla_423617(1).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423617(1).Total_Trans:= tabla_423617(1).Total_Trans + 1;
                tabla_423617(1).Total_Comisiones:= tabla_423617(1).Total_Comisiones + vTotal_Comisiones;
             ELSIF vSettlementFlag = '8' THEN
                tabla_423617(2).Total_Source_Amount:=tabla_423617(2).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423617(2).Total_Trans:= tabla_423617(2).Total_Trans + 1;
                tabla_423617(2).Total_Comisiones:= tabla_423617(2).Total_Comisiones + vTotal_Comisiones;
             END IF;
          ELSIF vUsage_Code = '2' THEN
             IF vSettlementFlag = '0' THEN
                tabla_423617(3).Total_Source_Amount:=tabla_423617(3).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423617(3).Total_Trans:= tabla_423617(3).Total_Trans + 1;
                tabla_423617(3).Total_Comisiones:= tabla_423617(3).Total_Comisiones + vTotal_Comisiones;
             ELSIF vSettlementFlag = '8' THEN
                tabla_423617(4).Total_Source_Amount:=tabla_423617(4).Total_Source_Amount + vTotal_Source_Amount;
                tabla_423617(4).Total_Trans:= tabla_423617(4).Total_Trans + 1;
                tabla_423617(4).Total_Comisiones:= tabla_423617(4).Total_Comisiones + vTotal_Comisiones;
             END IF;
          END IF; -- Usage Code = 2 and Moneda = 604
       END IF; -- TC 05
   --************ Para el Tc 06 **************************--
       IF vTc = '06' AND vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423617(5).Total_Source_Amount:=tabla_423617(5).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423617(5).Total_Trans:= tabla_423617(5).Total_Trans + 1;
             tabla_423617(5).Total_Comisiones:= tabla_423617(5).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423617(6).Total_Source_Amount:=tabla_423617(6).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423617(6).Total_Trans:= tabla_423617(6).Total_Trans + 1;
             tabla_423617(6).Total_Comisiones:= tabla_423617(6).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 06
   --************ Para el Tc 25 **************************--
       IF vTc = '25' AND vUsage_Code= '1' THEN
          IF vSettlementFlag = '0' THEN
             tabla_423617(7).Total_Source_Amount:=tabla_423617(7).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423617(7).Total_Trans:= tabla_423617(7).Total_Trans + 1;
             tabla_423617(7).Total_Comisiones:= tabla_423617(7).Total_Comisiones + vTotal_Comisiones;
          ELSIF vSettlementFlag = '8' THEN
             tabla_423617(8).Total_Source_Amount:=tabla_423617(8).Total_Source_Amount + vTotal_Source_Amount;
             tabla_423617(8).Total_Trans:= tabla_423617(8).Total_Trans + 1;
             tabla_423617(8).Total_Comisiones:= tabla_423617(8).Total_Comisiones + vTotal_Comisiones;
          END IF;
       END IF; -- TC 25
    END IF; -- If BIN = 423617
 END p_Update_Contadores;

---------------------------------------------------------------------

 PROCEDURE p_Inserta_10_20 (VFecPro    CHAR,
                           vBIN       VARCHAR2,
                           vTran      VARCHAR2,
                           vCurrencyC VARCHAR2,
                           vSett_Flag VARCHAR2,
                           vReasonC   VARCHAR2,
                           vSource_Am NUMBER)
 IS
 vSourceAmount NUMBER := 0;
 vNumTransac   NUMBER := 0;
 BEGIN
    SELECT SOURCE_AMOUNT, TRANS_NUMBER
    INTO vSourceAmount, vNumTransac
    FROM TC10_TC20
    WHERE FECHA_PROCESO   = VFecPro
      AND ACQUIRER_BIN    = VBIN
      AND TRANS_CODE      = vTran
      AND SOURCE_CURRENCY = vCurrencyC
      AND SETTLEMENT_FLAG = vSett_Flag
      AND REASON_CODE     = vReasonC;

    vSourceAmount := vSourceAmount + vSource_Am;
    vNumTransac := vNumTransac + 1;
    UPDATE TC10_TC20
       SET SOURCE_AMOUNT = vSourceAmount,
           TRANS_NUMBER = vNumTransac
     WHERE FECHA_PROCESO   = VFecPro
       AND ACQUIRER_BIN    = VBIN
       AND TRANS_CODE      = vTran
       AND SOURCE_CURRENCY = vCurrencyC
       AND SETTLEMENT_FLAG = vSett_Flag
       AND REASON_CODE     = vReasonC;
    COMMIT;
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
         INSERT INTO TC10_TC20
         VALUES (VFecPro,
                 VBIN,
                 vTran,
                 vSource_Am,
                 vCurrencyC,
                 vSett_Flag,
                 vReasonC,
                 1);
         COMMIT;
    WHEN OTHERS THEN
         RETURN;
 END p_Inserta_10_20;

---------------------------------------------------------------------

 FUNCTION f_NroOper(vFechad DATE,
                   vMoneda VARCHAR2) RETURN NUMBER
 IS
 vNroOper NUMBER := 0;
 BEGIN
    IF vMoneda = vCodMoneda Then
       SELECT COUNT(*)
         INTO  vNroOper
         FROM  CLR_RESUMEN
        WHERE FEC_SESION = vFechad
        GROUP BY COD_MONEDA;
    END IF;
    RETURN vNroOper;
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
         RETURN 0;
    WHEN TOO_MANY_ROWS THEN
         RETURN 0;
    WHEN OTHERS THEN
         RETURN 0;
 END f_NroOper;

---------------------------------------------------------------------

 FUNCTION f_MtoNeto(vFechad DATE,
                   vMoneda VARCHAR2) RETURN NUMBER
 IS
 vMtoNeto NUMBER := 0;
 BEGIN
    IF vMoneda = vCodMoneda  THEN
       SELECT SUM(IMP_NETO_SIGNO)
         INTO  vMtoNeto
         FROM  CLR_RESUMEN
        WHERE FEC_SESION = vFechad
        GROUP BY COD_MONEDA;
    END IF;
    RETURN vMtoNeto;
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
         RETURN 0;
    WHEN TOO_MANY_ROWS THEN
         RETURN 0;
    WHEN OTHERS THEN
         RETURN 0;
 END f_MtoNeto;

---------------------------------------------------------------------

 FUNCTION  f_buscap26(codact_p26_actc IN VARCHAR) RETURN VARCHAR
 IS
    vDES_MCC    VARCHAR2(50);
 BEGIN
    SELECT rtrim(ltrim(DES_MCC)) INTO vDES_MCC
      FROM mcc_pmp
     WHERE COD_MCC = codact_p26_actc;

    IF rtrim(ltrim(vDES_MCC))  is null THEN
       RETURN '            ';
    ELSE
       RETURN rpad( substr(rtrim(ltrim(upper(vDES_MCC))),1,12) , 12, ' ');
    END IF;
 END f_buscap26;

---------------------------------------------------------------------
FUNCTION atleastone_TID (
    COD_INT IN VARCHAR2
   )
   RETURN VARCHAR2
IS
   retval BOOLEAN;

         CURSOR category_cur IS
          SELECT
                    TID  as TransactionIdentifier
                  FROM
                  TID_VISA
                  WHERE REFADQ = COD_INT
       ;
        category_rec category_cur%ROWTYPE;
BEGIN


          OPEN category_cur;
          FETCH category_cur INTO category_rec;
          retval := category_cur%FOUND;
          CLOSE category_cur;

          --SELECT LPAD('0',15,'0') FROM DUAL

   IF retval then
    RETURN category_rec.TransactionIdentifier;
   ELSE
    RETURN LPAD('0',15,'0');
   END IF   ;
END;
 FUNCTION f_POS_Environment(punsr_p22 in varchar) RETURN VARCHAR
 IS
 BEGIN
    IF substr(rtrim(ltrim(punsr_p22)),5,1) = '3' THEN
       RETURN 'R';
    ELSE
       RETURN ' ';
    END IF;
 END f_POS_Environment;

---------------------------------------------------------------------

 FUNCTION f_Terminal_ID(vPUNSER_P22_ACTC in varchar,
                       vIDETER_P41_ACTC in varchar,
                       vREFADQ_P31_ACTC  in varchar,
                       vIDEADQ_P32_ACTC  in varchar) RETURN VARCHAR
 IS
 BEGIN
    IF substr(vPUNSER_P22_ACTC,1,1) = '2' OR substr(vPUNSER_P22_ACTC,1,1) = '5' THEN
       RETURN vIDETER_P41_ACTC;
    ELSE
       RETURN '        ';
    END IF;
 END f_Terminal_ID;

---------------------------------------------------------------------

 FUNCTION f_Cardhold_ActTer_Ind(vIDETER_P41_ACTC in varchar,
                                vvIDEADQ_P32_ACTC in varchar,
                                vTIPMOV_P48_ACTC in varchar) RETURN VARCHAR
 IS
 BEGIN
    IF substr(vIDETER_P41_ACTC,1,1)= '8' AND substr(vvIDEADQ_P32_ACTC,1,2) = '54' THEN
        -- Kiosko Electronico
       IF upper(vTIPMOV_P48_ACTC) = 'C' THEN --Tarjetas de Credito
        -- Operaciones tipo B
          RETURN '3';
       ELSE
          IF upper(vTIPMOV_P48_ACTC) = 'D' THEN -- Tarjeta de Debito,
           -- operaciones tipo C
             RETURN '2';
          ELSE
             RETURN ' ';
          END IF;
       END IF;
    ELSE
       RETURN ' ';
    END IF;
 END f_Cardhold_ActTer_Ind;

--**********************************************************************************************************
--**
--**********************************************************************************************************

PROCEDURE p_CargaRegRechazados_ventas (pCod_EntAdq varchar2,
                                       pFechac6 varchar2,
                                       pFile in out UTL_FILE.FILE_TYPE,
                                       vNum_Tcrs_tc91 in out number,
                                       vSource_Amount_tc91 in out number,
                                       vNum_Tcrs_tc92 in out number,
                                       vSource_Amount_tc92 in out number,
                                       vNum_MTrans_tc91 in out number,
                                       vNum_Trns_tc91 in out number,
                                       vNum_MTrans_tc92 in out number,
                                       vNum_Trns_tc92 in out number,
                                       vBatch_Number_tc91 in out number,
                                       vBatch_Number_tc92 in out number)
IS

---tcr_5  y tcr_7
VTTCRIP_P55_ACTC CLR_DCEMVFULL.ttcrip_p55_actc%TYPE;
VNUMSEC_P23_ACTC CLR_DCEMVFULL.numsec_p23_actc%TYPE;
VFECCRI_P55_ACTC CLR_DCEMVFULL.feccri_p55_actc%TYPE;
VCAPTER_P55_ACTC CLR_DCEMVFULL.capter_p55_actc%TYPE;
VPAICRI_P55_ACTC CLR_DCEMVFULL.paicri_p55_actc%TYPE;
VNUMALE_P55_ACTC CLR_DCEMVFULL.numale_p55_actc%TYPE;
VEMVATC_P55_ACTC CLR_DCEMVFULL.emvatc_p55_actc%TYPE;
VPERINT_P55_ACTC CLR_DCEMVFULL.perint_p55_actc%TYPE;
VCRIPPE_P55_ACTC CLR_DCEMVFULL.crippe_p55_actc%TYPE;
VDAPLEM_P55_ACTC CLR_DCEMVFULL.daplem_p55_actc%TYPE;
VEMVTVR_P55_ACTC CLR_DCEMVFULL.emvtvr_p55_actc%TYPE;
VIMPCRI_P55_ACTC CLR_DCEMVFULL.impcri_p55_actc%TYPE;
VSCREMI_P55_ACTC CLR_DCEMVFULL.scremi_p55_actc%TYPE;
VRESCRI_P55_ACTC CLR_DCEMVFULL.rescri_p55_actc%TYPE;

wkIdPart   CHAR(4);                /* OBTENER LA PARTICION */
wkIdMes   NUMBER;               /*  OBTENER LA SUBPARTICION */

    CURSOR Cr_VisaRechret IS
    SELECT id_rechret, cod_entadq, fila, fec_sesion, fec_upd, tipo_incoming, estado, observaciones, trn_code,
           trn_code_qlf, trn_comp_seq_num, account_num, account_num_ext, floor_limit_ind, crb_excep_file_ind,
           pcas_ind, acq_ref_num, acq_bus_id, purchase_date, source_amount, source_curren_code, merch_name,
           merch_city, merch_count_code, merch_cate_code, merch_zip_code, merch_prov_code, req_payment_serv,
           usage_code, request_reason_code, settlement_flag, aut_charact_ind, authorization_code, pos_capability,
           inter_fee_ind, cardholder_id_met, collec_only_flag, pos_entry_mode, central_proc_date, reimb_attribute,
           iss_workstat_bin, acq_workstat_bin, chargeback_ref_num, documentation_ind, member_mess_text, special_cond_ind_1,
           special_cond_ind_2, fee_program_ind, card_acceptor_id, terminal_id, national_reimb_fee, mail_phone_ind,
           spec_chargeback_ind, interface_trace_num, cardhold_actter_ind, prepaid_card_ind, serv_develop_field,
           avs_response_code, aut_source_code, purchase_iden_form, atm_account_select, inst_payment_count,
           purchase_identifier, cashback, chip_condition_code, pos_enviroment
      FROM VisaRechret
     WHERE FEC_UPD = to_date(pFechac6,'YYMMDD')
       AND ESTADO = 'T'
       AND cod_entadq = pCod_EntAdq
       AND trn_code not in ('10','20');

    kSCREMI_P55_ACTC_DEF CHAR(10):= '          ';
    reg_vr       Cr_VisaRechret%ROWTYPE;
    vTCR_0          TCR_0;
    vTCR_1       TCR_1;
    vTCR_5       TCR_5;
    vTCR_7       TCR_7;
    Linea        VARCHAR2(500);
    VCUENTA      NUMBER:=0;

    p_IDEMEN_P00 NUMBER(4);
    p_CODPRO_P03 VARCHAR2(6);
    p_IMPTRA_P04 VARCHAR2(12);
    p_IDETRA_P11 VARCHAR2(6);
    p_TIMLOC_P12 VARCHAR2(12);
    p_PUNSER_P22 VARCHAR2(12);
    p_IDEADQ_P32 VARCHAR2(6);
    p_IDETER_P41 VARCHAR2(8);
    p_MONTRA_P49 VARCHAR2(3);
    P_TID        VARCHAR2(15);

BEGIN

   PQMONPROC.inslog(nIdProceso,'M','Cod_EntAdq:'||pCod_EntAdq);
   PQMONPROC.inslog(nIdProceso,'M','vFechac6:'||pFechac6);

    OPEN Cr_VisaRechret;
    LOOP
        BEGIN
            FETCH Cr_VisaRechret INTO reg_vr;
            EXIT WHEN Cr_VisaRechret%NOTFOUND;

            PQMONPROC.inslog(nIdProceso,'M','reg_vr.trn_code:'||reg_vr.trn_code);

                vTCR_0.Trn_Code := reg_vr.trn_code;
                vTCR_0.Trn_Code_Qlf := '0';
                vTCR_0.Trn_Comp_Seq_Num := '0';
                vTCR_0.Account_Num := rpad(reg_vr.account_num,19,'0');
                vTCR_0.Floor_Limit_Ind:=' ';
                vTCR_0.CRB_Excep_File_Ind := ' ';
                vTCR_0.PCAS_ind := ' ';
                vTCR_0.Acq_Ref_Num := reg_vr.acq_ref_num;
                vTCR_0.Acq_Bus_ID := '00000000';
                vTCR_0.Purchase_date :=reg_vr.purchase_date;
                vTCR_0.Dest_Amount := '000000000000';
                vTCR_0.Dest_Curren_Code := '   ';
                vTCR_0.Source_Amount :=reg_vr.source_amount; -- numerico
                vTCR_0.Source_Curren_Code := reg_vr.source_curren_code;
                vTCR_0.Merch_Name := rpad(nvl(reg_vr.merch_name,' '),25,' ');
                vTCR_0.Merch_city := rpad(nvl(reg_vr.merch_city,' '), 13, ' ');
                vTCR_0.Merch_Count_Code := rpad(nvl(reg_vr.merch_count_code,' '),3, ' ');
                vTCR_0.Merch_Cate_Code  := rpad(nvl(reg_vr.merch_cate_code,' '),4, ' ');
                vTCR_0.Merch_Zip_Code   := '00000';
                vTCR_0.Merch_Prov_Code  := '   ';
                vTCR_0.Req_Payment_Serv :=' ';
                vTCR_0.Reserved_01 := ' ' ;
                vTCR_0.Usage_Code := rpad(nvl(reg_vr.usage_code,' '), 1, ' ');
                vTCR_0.Reason_Code  := nvl(reg_vr.request_reason_code,'00');
                vTCR_0.Settlement_Flag  := reg_vr.settlement_flag;
                vTCR_0.Aut_Charact_Ind := ' '; --reg_vr.aut_charact_ind;
                vTCR_0.Authorization_Code := reg_vr.authorization_code;
                vTCR_0.POS_Capability  := reg_vr.pos_capability;
                vTCR_0.Cardholder_ID_Met := reg_vr.cardholder_id_met;
                vTCR_0.Collec_Only_Flag :=' ';
                vTCR_0.POS_Entry_Mode := reg_vr.pos_entry_mode;
                vTCR_0.Inter_Fee_Ind := reg_vr.inter_fee_ind;
                IF vTCR_0.Trn_Code  in ('25','26') THEN
                    vTCR_0.Central_Proc_Date := reg_vr.central_proc_date;
                ELSE
                    vTCR_0.Central_Proc_Date := '0000'; --reg_vr.central_proc_date;
                END IF;
                vTCR_0.Reimb_Attribute := reg_vr.reimb_attribute;

                linea := lpad(vTCR_0.Trn_Code,2,'0')                || vTCR_0.Trn_Code_Qlf             || vTCR_0.Trn_Comp_Seq_Num        ||
                         vTCR_0.Account_Num                         || vTCR_0.Floor_Limit_Ind          || vTCR_0.CRB_Excep_File_Ind      ||
                         vTCR_0.PCAS_ind                            || vTCR_0.Acq_Ref_Num              || lpad(vTCR_0.Acq_Bus_ID,8,'0')  ||
                         vTCR_0.Purchase_date                       || lpad(vTCR_0.Dest_Amount,12,'0') || vTCR_0.Dest_Curren_Code        ||
                         lpad(to_char(vTCR_0.Source_Amount),12,'0') || vTCR_0.Source_Curren_Code       || vTCR_0.Merch_Name              ||
                         vTCR_0.Merch_city                          || vTCR_0.Merch_Count_Code         || vTCR_0.Merch_Cate_Code         ||
                         vTCR_0.Merch_Zip_Code                      || vTCR_0.Merch_Prov_Code          || vTCR_0.Req_Payment_Serv        ||
                         vTCR_0.Reserved_01                         || vTCR_0.Usage_Code               || lpad(vTCR_0.Reason_Code,2,'0') ||
                         vTCR_0.Settlement_Flag                     || vTCR_0.Aut_Charact_Ind          || vTCR_0.Authorization_Code      ||
                         vTCR_0.POS_Capability                      || vTCR_0.Inter_Fee_Ind            || vTCR_0.Cardholder_ID_Met       ||
                         vTCR_0.Collec_Only_Flag                    || vTCR_0.POS_Entry_Mode           || vTCR_0.Central_Proc_Date       ||
                         vTCR_0.Reimb_Attribute;

                pqOutput.PUT_LINE(linea);
                --  Generacion de OUTVIYYYYMMDD.TXT
                IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                    utl_file.PUT_LINE( pFile, linea);
                ELSE
                    IF vTCR_0.Trn_Code IN ('06','25') THEN
                        utl_file.PUT_LINE( pFile, linea);
                    END IF;
                END IF;
                --  Fin de Generacion de OUTVIYYYYMMDD.TXT

                -- **** p_Update_Contadores(SUBSTR(vTCR_0.Acq_Ref_Num,2,6), vtcr_0.Trn_Code, vtcr_0.Usage_Code, vtcr_0.Source_Curren_Code, vtcr_0.Settlement_Flag, vtcr_0.Source_Amount, rCr_EX8001.IMPCU1_P46_ACTC);

                -- Contadores para TC1
                vNum_Tcrs_tc91:= vNum_Tcrs_tc91+1;
                vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_0.Source_Amount;
                vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;
                vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_0.Source_Amount;

                -- Aqui falta algo mas Revisar

                vTCR_1.Trn_Code             := lpad(reg_vr.trn_code,2,'0');
                vTCR_1.Trn_Code_Qlf         :='0';
                vTCR_1.Trn_Comp_Seq_Num     :='1';
                vTCR_1.Iss_Workstat_BIN     := '      ';
                vTCR_1.Acq_Workstat_BIN     := '      ';
                vTCR_1.Chargeback_Ref_Num   := lpad(reg_vr.chargeback_ref_num,6,'0');
                vTCR_1.Documentation_Ind    := reg_vr.documentation_ind;
                vTCR_1.Member_Mess_Text     := rpad(reg_vr.member_mess_text,50,' ');
                vTCR_1.Special_Cond_Ind     := '  ';
                vTCR_1.FeeProgramID         := '   ';
                vTCR_1.Issuer_charge        := ' ';
                vTCR_1.Reserved_01          := ' ';
                vTCR_1.Card_Acceptor_ID     := lpad(reg_vr.card_acceptor_id, 15,'0');
              --vTCR_1.Terminal_ID := rCr_EX8001.IDETER_P41_ACTC;--Cambio pedido de AV 06 SEP 2004
                vTCR_1.Terminal_ID          := rpad(nvl(reg_vr.terminal_id,' '),8,' ');
                vTCR_1.National_Reimb_Fee   := f_Nat_Reimbursement_fee_06(vTCR_0.Settlement_Flag,reg_vr.national_reimb_fee); --reg_vr.national_reimb_fee;
                vTCR_1.Mail_Phone_Ind       := reg_vr.mail_phone_ind;
                vTCR_1.Spec_Chargeback_Ind := ' ';
                vTCR_1.Interface_Trace_Num := '000000';
            --  vTCR_1.Cardhold_ActTer_Ind := ' '; --comentado pedido AV
                vTCR_1.Cardhold_ActTer_Ind := reg_vr.cardhold_actter_ind;
                vTCR_1.Prepaid_Card_Ind := ' ';
                vTCR_1.Reserved_02 := ' ';
                vTCR_1.AVS_Response_Code := ' ';
                vTCR_1.Aut_Source_Code := ' ';
                vTCR_1.Purchase_Iden_Form := ' ';
                vTCR_1.ATM_Account_Select := '0';
                vTCR_1.Inst_Payment_Count := '  ';
                vTCR_1.Purchase_Identifier := '                         ';
             -- vTCR_1.Cashback := '000000000';
                vTCR_1.Cashback := lpad(nvl(reg_vr.cashback,0),9,'0');
                vTCR_1.Chip_Cond_Code := reg_vr.chip_condition_code;
                vTCR_1.POS_Environment  := reg_vr.pos_enviroment;

                Linea := vTCR_1.Trn_Code                        ||   vTCR_1.Trn_Code_Qlf                    ||  vTCR_1.Trn_Comp_Seq_Num                 ||
                         vTCR_1.Iss_Workstat_BIN                ||   vTCR_1.Acq_Workstat_BIN                ||  vTCR_1.Chargeback_Ref_Num               ||
                         vTCR_1.Documentation_Ind               ||   vTCR_1.Member_Mess_Text                ||  vTCR_1.Special_Cond_Ind                 ||
                         vTCR_1.FeeProgramID    ||  vTCR_1.Issuer_charge ||
                         vTCR_1.Reserved_01                     ||   vTCR_1.Card_Acceptor_ID                ||  vTCR_1.Terminal_ID                      ||
                         lpad(vTCR_1.National_Reimb_Fee,12,'0') ||   vTCR_1.Mail_Phone_Ind                  ||  vTCR_1.Spec_Chargeback_Ind              ||
                         vTCR_1.Interface_Trace_Num             ||   vTCR_1.Cardhold_ActTer_Ind             ||  vTCR_1.Prepaid_Card_Ind                 ||
                         vTCR_1.Reserved_02                     ||   vTCR_1.AVS_Response_Code               ||  vTCR_1.Aut_Source_Code                  ||
                         vTCR_1.Purchase_Iden_Form              ||   vTCR_1.ATM_Account_Select              ||  vTCR_1.Inst_Payment_Count               ||
                       --vTCR_1.Purchase_Identifier || vTCR_1.Cashback ||  vTCR_1.Chip_Cond_Code ||  vTCR_1.POS_Environment;
                         vTCR_1.Purchase_Identifier              ||   vTCR_1.Cashback                        ||  vTCR_1.Chip_Cond_Code                   ||
                         vTCR_1.POS_Environment;
                pqOutput.PUT_LINE(linea);
                --  Generacion de outviEEEEyyyymmdd.txt
                IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                    utl_file.PUT_LINE( pFile, linea);
                ELSE
                   IF vTCR_0.Trn_Code IN ('06','25','26') THEN
                      utl_file.PUT_LINE( pFile , linea);
                   END IF;
                END IF;
            -- Contadores para tc91 y 92 del Tc07
               vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
               vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's
-----------------
--vTCR_5 y vTCR_7
-----------------

/*   IF reg_vr.chip_condition_code='1' THEN

      SELECT IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC, IDETRA_P11_ACTC,TIMLOC_P12_ACTC,IDEADQ_P32_ACTC,IDETER_P41_ACTC, MONTRA_P49_ACTC
        INTO p_IDEMEN_P00   ,p_CODPRO_P03   ,p_IMPTRA_P04   ,p_IDETRA_P11   ,p_TIMLOC_P12   ,p_IDEADQ_P32   ,p_IDETER_P41    ,p_MONTRA_P49
        FROM CLR_EX8001
       WHERE REFADQ_P31_ACTC =trim(reg_vr.acq_ref_num);
*/
      IF reg_vr.cod_entadq = 'BM' THEN

        SELECT p00idmsg   ,lpad(p03codpro,6,'0'),p04imptra    ,LPAD(LTRIM(p11idetra),6,'0')    ,TO_CHAR(TO_DATE(RTRIM(P12TIMLOC),'YYYYMMDDHH24MISS'),'YYMMDDHH24MISS'), p22punser, p32idadq     ,p41cseri     ,p49montra
                     ,idparticion||SUBSTR(p28sesion,5,2)||TO_CHAR(TO_DATE(p28sesion,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion,id_mes
        INTO p_IDEMEN_P00 ,p_CODPRO_P03         ,p_IMPTRA_P04 ,p_IDETRA_P11 ,p_TIMLOC_P12 ,p_PUNSER_P22, p_IDEADQ_P32 ,p_IDETER_P41 ,p_MONTRA_P49,wkIdPart,wkIdMes
        FROM MCP_BM
        WHERE  p00idmsg = decode(lpad(reg_vr.trn_code,2,'0'),'05','1244','06','1244','25','1444')
         AND p31refadq = trim(reg_vr.acq_ref_num);


      ELSIF reg_vr.cod_entadq = 'BP' THEN

        SELECT p00idmsg   ,lpad(p03codpro,6,'0'),p04imptra    ,LPAD(LTRIM(p11idetra),6,'0')    ,TO_CHAR(TO_DATE(RTRIM(P12TIMLOC),'YYYYMMDDHH24MISS'),'YYMMDDHH24MISS'), p22punser, p32idadq     ,p41cseri     ,p49montra
                   ,idparticion||SUBSTR(p28sesion,5,2)||TO_CHAR(TO_DATE(p28sesion,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion,id_mes
        INTO p_IDEMEN_P00 ,p_CODPRO_P03         ,p_IMPTRA_P04 ,p_IDETRA_P11 ,p_TIMLOC_P12 ,p_PUNSER_P22, p_IDEADQ_P32 ,p_IDETER_P41 ,p_MONTRA_P49,wkIdPart,wkIdMes
         FROM MCP_BP
        WHERE  p00idmsg = decode(lpad(reg_vr.trn_code,2,'0'),'05','1244','06','1244','25','1444')
         AND p31refadq = trim(reg_vr.acq_ref_num);

      END IF;

      IF SUBSTR(p_PUNSER_P22,7,1) = '5' THEN


          SELECT COUNT(1)
             INTO VCUENTA
             FROM CLR_DCEMVFULL
             WHERE IDEMEN_P00_ACTC = p_IDEMEN_P00     --WHERE IDEMEN_P00_ACTC = DECODE(p_IDEMEN_P00,'1240','1244','1440','1444')
              AND IDETRA_P11_ACTC = p_IDETRA_P11
              AND TIMLOC_P12_ACTC = p_TIMLOC_P12
              AND IDEADQ_P32_ACTC = p_IDEADQ_P32
              AND idparticion =wkIdPart  /*implementacion del particionamiento*/
              AND id_mes     =wkIdMes;  /*implementacion del particionamiento*/

          IF VCUENTA > 0  THEN

               SELECT TTCRIP_P55_ACTC,NUMSEC_P23_ACTC,FECCRI_P55_ACTC,CAPTER_P55_ACTC,
                      PAICRI_P55_ACTC,NUMALE_P55_ACTC,EMVATC_P55_ACTC,PERINT_P55_ACTC,
                      CRIPPE_P55_ACTC,DAPLEM_P55_ACTC,EMVTVR_P55_ACTC,IMPCRI_P55_ACTC,
                      SCREMI_P55_ACTC,RESCRI_P55_ACTC
                 INTO VTTCRIP_P55_ACTC,VNUMSEC_P23_ACTC,VFECCRI_P55_ACTC,VCAPTER_P55_ACTC,
                      VPAICRI_P55_ACTC,VNUMALE_P55_ACTC,VEMVATC_P55_ACTC,VPERINT_P55_ACTC,
                      VCRIPPE_P55_ACTC,VDAPLEM_P55_ACTC,VEMVTVR_P55_ACTC,VIMPCRI_P55_ACTC,
                      VSCREMI_P55_ACTC,VRESCRI_P55_ACTC
                 FROM CLR_DCEMVFULL
                 WHERE IDEMEN_P00_ACTC = p_IDEMEN_P00      --WHERE IDEMEN_P00_ACTC = DECODE(p_IDEMEN_P00,'1240','1244','1440','1444')
                  AND IDETRA_P11_ACTC = p_IDETRA_P11
                  AND TIMLOC_P12_ACTC = p_TIMLOC_P12
                  AND IDEADQ_P32_ACTC = p_IDEADQ_P32
                  AND idparticion =wkIdPart    /*implementacion del particionamiento*/
                  AND id_mes     =wkIdMes;    /*implementacion del particionamiento*/




                  P_TID := SGCVNZ.PQOUTGOINGVISA.atleastone_TID(trim(reg_vr.acq_ref_num));



             vTCR_5.Trn_Code                       := lpad(reg_vr.trn_code,2,'0');--- f_Trn_Code(p_IDEMEN_P00,p_CODPRO_P03,NULL);
             vTCR_5.Trn_Code_Qlf                   := '0';
             vTCR_5.Trn_Comp_Seq_Num               := '5';
             vTCR_5.TransactionIdentifier          := P_TID; --NVL(substr(rCr_EX8001.TransactionIdentifier,1,15), LPAD('0',15,'0')); --carta tecnica visa 2013 inclusin del TID--LPAD('0',15,'0'); --verificar rCr_EX8001.TransactionIdentifier
             --dbms_output.put_line('ENTRO 3:' || vTCR_5.TransactionIdentifier);
             vTCR_5.AuthorizedAmount               := LPAD(TO_CHAR(p_IMPTRA_P04),12,'0');
             vTCR_5.AuthorizationCurrencyCode      := LPAD(p_MONTRA_P49,3,' ');
             vTCR_5.AuthorizationResponseCode      := LPAD(NVL(TRIM(VRESCRI_P55_ACTC),0),2,'0'); --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0');  --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0'); --LPAD(VRESCRI_P55_ACTC,2,'0');
             vTCR_5.ValidationCode                 := LPAD(' ',4,' ');
             vTCR_5.ExcludedTranIdenReason         := LPAD(' ',1,' ');
             vTCR_5.CRSProcessingCode              := LPAD(' ',1,' ');
             vTCR_5.ChargebackRightsIndicator      := LPAD(' ',2,' ');
             vTCR_5.MultipleClearingSequenceNumber := LPAD('0',2,'0');
             vTCR_5.MultipleClearingSequenceCount  := LPAD('0',2,'0');
             vTCR_5.MarketSpecificAutDataInd       := LPAD(' ',1,' ');
             vTCR_5.TotalAuthorizedAmount          := LPAD('0',12,'0');
             vTCR_5.InformationIndicator           := LPAD(' ',1,' ');
             vTCR_5.MerchantTelephoneNumber        := LPAD(' ',14,' ');
             vTCR_5.AdditionalDataIndicator        := LPAD(' ',1,' ');
             vTCR_5.MerchantVolumeIndicator        := LPAD(' ',2,' ');
             vTCR_5.ElectronicCommGoodsInd         := LPAD(' ',2,' ');
             vTCR_5.MerchantVerificationValue      := LPAD(' ',10,' ');
             vTCR_5.InterchangeFeeAmount           := LPAD('0',15,'0');
             vTCR_5.InterchangeFeeSign             := LPAD(' ',1,' ');
             vTCR_5.SourceCurrBaseCurrExchRate     := LPAD('0',8,'0');
             vTCR_5.BaseCurrtoDestCurrExchRate     := LPAD('0',8,'0');
             vTCR_5.OptionalIssuerISAAmount        := LPAD('0',12,'0');
             vTCR_5.ProductID                      := LPAD(' ',2,' ');
             vTCR_5.ProgramID                      := LPAD(' ',6,' ');
             vTCR_5.Reserved                       := LPAD(' ',24,' ');
             vTCR_5.CVV2ResultCode                 := LPAD(' ',1,' ');


            IF TRIM(vTCR_5.ProductID) = 'I' THEN
                 vTCR_5.ProductID := 'I1';
             END IF;

             linea := LPAD(vTCR_5.Trn_Code,2,'0')||vTCR_5.Trn_Code_Qlf||
                      vTCR_5.Trn_Comp_Seq_Num||vTCR_5.TransactionIdentifier||
                      vTCR_5.AuthorizedAmount||vTCR_5.AuthorizationCurrencyCode||
                      vTCR_5.AuthorizationResponseCode||vTCR_5.ValidationCode||
                      vTCR_5.ExcludedTranIdenReason||vTCR_5.CRSProcessingCode||
                      vTCR_5.ChargebackRightsIndicator||vTCR_5.MultipleClearingSequenceNumber||
                      vTCR_5.MultipleClearingSequenceCount||vTCR_5.MarketSpecificAutDataInd||
                      vTCR_5.TotalAuthorizedAmount||vTCR_5.InformationIndicator||
                      vTCR_5.MerchantTelephoneNumber||vTCR_5.AdditionalDataIndicator||
                      vTCR_5.MerchantVolumeIndicator||vTCR_5.ElectronicCommGoodsInd||
                      vTCR_5.MerchantVerificationValue||vTCR_5.InterchangeFeeAmount||
                      vTCR_5.InterchangeFeeSign||vTCR_5.SourceCurrBaseCurrExchRate||
                      vTCR_5.BaseCurrtoDestCurrExchRate||vTCR_5.OptionalIssuerISAAmount||
                      vTCR_5.ProductID||vTCR_5.ProgramID||
                      vTCR_5.Reserved||
                      vTCR_5.CVV2ResultCode;

             --dbms_output.put_line('ENTRO 3 det : ' || linea);

             pqOutput.PUT_LINE(linea);

             vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;
             vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;


             vTCR_7.Trn_Code := lpad(reg_vr.trn_code,2,'0'); --- f_Trn_Code(p_IDEMEN_P00,p_CODPRO_P03,NULL);
             vTCR_7.Trn_Code_Qlf := '0';
             vTCR_7.Trn_Comp_Seq_Num := '7';
             vTCR_7.dctcttcrip := LPAD(NVL(VTTCRIP_P55_ACTC,'0'),2,'0');
             vTCR_7.dcnumsec := LPAD(NVL(VNUMSEC_P23_ACTC,0),3,'0');
             vTCR_7.dctcfeccri := LPAD(VFECCRI_P55_ACTC,6,'0');
             vTCR_7.dctccapter := LPAD(VCAPTER_P55_ACTC,6,'0');
             vTCR_7.dctcpaicri :=  '862'; --RPAD(VPAICRI_P55_ACTC,3,' ');
             vTCR_7.Num_serie :=  LPAD(' ',8,' '); --LPAD(p_IDETER_P41,8,' ');
             vTCR_7.dctcnumale := LPAD(VNUMALE_P55_ACTC,8,'0');
             vTCR_7.dctcemvatc := LPAD(VEMVATC_P55_ACTC,4,'0');
             vTCR_7.dctcperint := LPAD(VPERINT_P55_ACTC,4,'0');
             vTCR_7.dctccrippe := LPAD(VCRIPPE_P55_ACTC,16,'0');
             vTCR_7.dctcdaplem01 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),3,2),2,'0');
             vTCR_7.dctcdaplem02 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),5,2),2,'0');
             vTCR_7.dctcemvtvr := LPAD(VEMVTVR_P55_ACTC,10,'0');
             vTCR_7.dctcdaplem03 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),7,8),8,'0');
             vTCR_7.dctcimpcri := LPAD(VIMPCRI_P55_ACTC,12,'0');
             vTCR_7.dctcdaplem04 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),15,2),2,'0');
             vTCR_7.dctcdaplem05 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),17,16),16,'0');
             vTCR_7.dctcdaplem06 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),1,2),2,'0');
             vTCR_7.dctcdaplem07 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),33,2),2,'0');
             vTCR_7.dctcdaplem08 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),35,30),30,'0');
             vTCR_7.Espacios := LPAD(' ',8,' ');
             vTCR_7.dctcscremi := NVL(LPAD(STD.F_RTRIM4HEX(VSCREMI_P55_ACTC),10,'0'),kSCREMI_P55_ACTC_DEF);

             linea := LPAD(vTCR_7.Trn_Code,2,'0') || vTCR_7.Trn_Code_Qlf || vTCR_7.Trn_Comp_Seq_Num ||
                      vTCR_7.dctcttcrip || LPAD(vTCR_7.dcnumsec,3,'0')|| vTCR_7.dctcfeccri||vTCR_7.dctccapter||
                      vTCR_7.dctcpaicri||vTCR_7.Num_serie||vTCR_7.dctcnumale||vTCR_7.dctcemvatc||
                      vTCR_7.dctcperint||vTCR_7.dctccrippe||vTCR_7.dctcdaplem01||vTCR_7.dctcdaplem02||
                      vTCR_7.dctcemvtvr||vTCR_7.dctcdaplem03||vTCR_7.dctcimpcri||vTCR_7.dctcdaplem04||
                      vTCR_7.dctcdaplem05||vTCR_7.dctcdaplem06||vTCR_7.dctcdaplem07||vTCR_7.dctcdaplem08||
                      vTCR_7.Espacios||vTCR_7.dctcscremi;

            pqOutput.PUT_LINE(linea);

           vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
           vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's

          END IF;

   END IF;


            --  Fin de Generacion de OutviEEEEyyyymmdd.txt
            -- ********** Para el TC91 ************
--                vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
                vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
                vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
--                vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's
                vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
                vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                IF vNum_MTrans_tc91 = 500 THEN
                   vNum_Tcrs_tc91 := vNum_Tcrs_tc91+1;--Numero de TCR's..91
                   vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's..92
                   vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                   vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                   vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                   vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                   linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                            vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,pFechac6) ;
                    pqOutput.PUT_LINE(linea);
                    vNum_MTrans_tc91 := 0;
                    vNum_Tcrs_tc91 := 0;
                    vNum_Trns_tc91 := 0;
                    vSource_Amount_tc91 := 0;
                END IF;

        EXCEPTION
           WHEN Others THEN
                dbms_output.put_line('Error en cursor Visa Rechazados:' || SQLERRM);
                RETURN;
        END;
    END LOOP;

END;

PROCEDURE p_CargaRegRechazados (pCod_EntAdq varchar2,
                                pFechac6 varchar2,
                                pFile in out UTL_FILE.FILE_TYPE,
                                vNum_Tcrs_tc91 in out number,
                                vSource_Amount_tc91 in out number,
                                vNum_Tcrs_tc92 in out number,
                                vSource_Amount_tc92 in out number,
                                vNum_MTrans_tc91 in out number,
                                vNum_Trns_tc91 in out number,
                                vNum_MTrans_tc92 in out number,
                                vNum_Trns_tc92 in out number,
                                vBatch_Number_tc91 in out number,
                                vBatch_Number_tc92 in out number)
IS

    CURSOR Cr_VisaRechret IS
    SELECT id_rechret, cod_entadq, fila, fec_sesion, fec_upd, tipo_incoming, estado, observaciones, trn_code,
           trn_code_qlf, trn_comp_seq_num, account_num, account_num_ext, floor_limit_ind, crb_excep_file_ind,
           pcas_ind, acq_ref_num, acq_bus_id, purchase_date, source_amount, source_curren_code, merch_name,
           merch_city, merch_count_code, merch_cate_code, merch_zip_code, merch_prov_code, req_payment_serv,
           usage_code, request_reason_code, settlement_flag, aut_charact_ind, authorization_code, pos_capability,
           inter_fee_ind, cardholder_id_met, collec_only_flag, pos_entry_mode, central_proc_date, reimb_attribute,
           iss_workstat_bin, acq_workstat_bin, chargeback_ref_num, documentation_ind, member_mess_text, special_cond_ind_1,
           special_cond_ind_2, fee_program_ind, card_acceptor_id, terminal_id, national_reimb_fee, mail_phone_ind,
           spec_chargeback_ind, interface_trace_num, cardhold_actter_ind, prepaid_card_ind, serv_develop_field,
           avs_response_code, aut_source_code, purchase_iden_form, atm_account_select, inst_payment_count,
           purchase_identifier, cashback, chip_condition_code, pos_enviroment
      FROM VisaRechret
     WHERE FEC_UPD = to_date(pFechac6,'YYMMDD')
       AND ESTADO = 'T'
       AND cod_entadq = pCod_EntAdq
       AND trn_code in ('10','20');

    reg_vr      Cr_VisaRechret%ROWTYPE;
    vTCR_10_20  TCR_10_20;
    Linea       varchar2(500);
    dfecha8     date;
     P_TID        VARCHAR2(15);


BEGIN

   PQMONPROC.inslog(nIdProceso,'M','Cod_EntAdq:'||pCod_EntAdq);
   PQMONPROC.inslog(nIdProceso,'M','vFechac6:'||pFechac6);

    OPEN Cr_VisaRechret;
    LOOP
        BEGIN
            FETCH Cr_VisaRechret INTO reg_vr;
            EXIT WHEN Cr_VisaRechret%NOTFOUND;

            PQMONPROC.inslog(nIdProceso,'M','reg_vr.trn_code:'||reg_vr.trn_code);
               vTCR_10_20.Trn_Code := reg_vr.trn_code;
               vTCR_10_20.Trn_Code_Qlf:= '0';
               vTCR_10_20.Trn_Comp_Seq_Num:=  '0';
               vTCR_10_20.Destination_BIN := '000000';
               IF reg_vr.cod_entadq = 'BM' THEN
                  vTCR_10_20.Source_BIN := '411032';
               ELSIF reg_vr.cod_entadq = 'BP' THEN
                  vTCR_10_20.Source_BIN := '420198';
               END IF;
               vTCR_10_20.Reason_Code := reg_vr.REQUEST_REASON_CODE;
               vTCR_10_20.Country_Code := f_Country_Code(vTCR_10_20.Reason_Code);
                vTCR_10_20.Event_Date:= SUBSTR(pFechac6,3,4);
               vTCR_10_20.Account_Num := reg_vr.account_num;
               vTCR_10_20.Dest_Amount:= 0;
               vTCR_10_20.Dest_Curren_Code := '   ';
               vTCR_10_20.Source_Amount := reg_vr.source_amount;
               vTCR_10_20.Source_Curren_Code:= reg_vr.source_curren_code;
               vTCR_10_20.Mess_Text:= RPAD(reg_vr.MEMBER_MESS_TEXT,70,' ');
               vTCR_10_20.Settlement_Flag := reg_vr.SETTLEMENT_FLAG;
               vTCR_10_20.Trans_Identifier:=  SGCVNZ.PQOUTGOINGVISA.atleastone_TID(reg_vr.ACQ_REF_NUM); -- '0'; --linea modificada felipe vitake
               vTCR_10_20.Reserved_01:= ' ';
               vTCR_10_20.Central_Proc_Date:= '0000';
               vTCR_10_20.Reimb_Attribute:='0';

               linea := vTCR_10_20.Trn_Code ||  vTCR_10_20.Trn_Code_Qlf ||   vTCR_10_20.Trn_Comp_Seq_Num||
                        lpad(vTCR_10_20.Destination_BIN,6,'0') || lpad(vTCR_10_20.Source_BIN,6,'0') ||   lpad(vTCR_10_20.Reason_Code,4,'0') ||
                        vTCR_10_20.Country_Code || lpad(vTCR_10_20.Event_Date,4,'0') ||   vTCR_10_20.Account_Num ||
                        lpad(vTCR_10_20.Dest_Amount,12,'0') || vTCR_10_20.Dest_Curren_Code ||
                        lpad(vTCR_10_20.Source_Amount,12,'0') || vTCR_10_20.Source_Curren_Code || vTCR_10_20.Mess_Text ||
                        vTCR_10_20.Settlement_Flag || LPAD(vTCR_10_20.Trans_Identifier,15,'0') ||  vTCR_10_20.Reserved_01 ||
                        vTCR_10_20.Central_Proc_Date || vTCR_10_20.Reimb_Attribute;

               pqOutput.PUT_LINE(linea);

               utl_file.PUT_LINE( pFile, linea);

               p_Update_Contadores(vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, '1', vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.Source_Amount, 0);
               dfecha8 := TO_DATE(pFechac6,'YYMMDD');
               p_Inserta_10_20(TO_CHAR(dfecha8,'YYYYMMDD'), vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.reason_code, vTCR_10_20.Source_Amount);

               --**********Para el TC91************
               vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_10_20.Source_Amount;
               vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_10_20.Source_Amount;
               vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
               vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
               vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
               vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's
               vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
               vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
               IF vNum_MTrans_tc91 = 500 THEN
                  vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
                  vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
                  vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                  vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                  vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                  vBatch_Number_tc92 := vBatch_Number_tc92 + 1;

                  linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                           vBatch_Number_tc91,vNum_Tcrs_tc91,
                           vNum_Trns_tc91,vSource_Amount_tc91,pFechac6) ;

                  pqOutput.PUT_LINE(linea);

                  vNum_MTrans_tc91 := 0;
                  vNum_Tcrs_tc91 :=0;
                  vNum_Trns_tc91 := 0;
                  vSource_Amount_tc91:= 0;
               END IF;

        EXCEPTION
           WHEN Others THEN
                dbms_output.put_line('Error en cursor Visa Rechazados:' || SQLERRM);
                RETURN;
        END;
    END LOOP;

END;
--------------------------------------------------------------------------------

 PROCEDURE p_OutGoingVisa_rev(pFecha VARCHAR, pBanco VARCHAR2)
 IS

 kSCREMI_P55_ACTC_DEF CHAR(10):= '          ';
 kDirOUT_Alias    VARCHAR2(25):='DIR-OUT';
 kDirOUT_Alias2   VARCHAR2(100):=STD.F_GETVALPAR('DIR-OUT');
 vFileOutVi          VARCHAR2(23);
 vFileOutVi1       VARCHAR2(23);

 file_id1         UTL_FILE.FILE_TYPE;

 pR_CodRes        VARCHAR2(10):='0000';
 pR_DesRes        VARCHAR2(200):= NULL;

 vTCR_0              TCR_0;
 vTCR_1           TCR_1;
 vTCR_5           TCR_5;
 vTCR_10_20       TCR_10_20;
 vTCR_7           TCR_7;

-- cont             NUMBER:=0;
 VCUENTA          NUMBER:=0;
 linea            VARCHAR2(500);

 vC               VARCHAR(50);
 vSQLError        NUMBER;
 vSqlErrM         VARCHAR2(200);
 r_Error          EXCEPTION;

 vFechad          DATE;
 vFechac          CHAR(8);
 vFechac6         CHAR(6);
 vFecha10         DATE;

 vCtaSelec        VARCHAR2(1);
 vNroOperBol      NUMBER:=0;
 vMtoNetoBol      NUMBER:=0;

 --Variables para generacion de TC91/92
 vNum_MTrans_tc91     NUMBER:=0;
 vBatch_Number_tc91   NUMBER:=0;
 vNum_Tcrs_tc91       NUMBER:=0;
 vNum_Trns_tc91       NUMBER:=0;
 vSource_Amount_tc91  NUMBER:=0;
 vNum_MTrans_tc92     NUMBER:=0;
 vBatch_Number_tc92   NUMBER:=0;
 vNum_Tcrs_tc92       NUMBER:=0;
 vNum_Trns_tc92       NUMBER:=0;
 vSource_Amount_tc92  NUMBER:=0;

varCashback          CHAR(9);

 --TCR_5 y TCR_7
 VTTCRIP_P55_ACTC CLR_DCEMVFULL.ttcrip_p55_actc%TYPE;
 VNUMSEC_P23_ACTC CLR_DCEMVFULL.numsec_p23_actc%TYPE;
 VFECCRI_P55_ACTC CLR_DCEMVFULL.feccri_p55_actc%TYPE;
 VCAPTER_P55_ACTC CLR_DCEMVFULL.capter_p55_actc%TYPE;
 VPAICRI_P55_ACTC CLR_DCEMVFULL.paicri_p55_actc%TYPE;
 VNUMALE_P55_ACTC CLR_DCEMVFULL.numale_p55_actc%TYPE;
 VEMVATC_P55_ACTC CLR_DCEMVFULL.emvatc_p55_actc%TYPE;
 VPERINT_P55_ACTC CLR_DCEMVFULL.perint_p55_actc%TYPE;
 VCRIPPE_P55_ACTC CLR_DCEMVFULL.crippe_p55_actc%TYPE;
 VDAPLEM_P55_ACTC CLR_DCEMVFULL.daplem_p55_actc%TYPE;
 VEMVTVR_P55_ACTC CLR_DCEMVFULL.emvtvr_p55_actc%TYPE;
 VIMPCRI_P55_ACTC CLR_DCEMVFULL.impcri_p55_actc%TYPE;
 VSCREMI_P55_ACTC CLR_DCEMVFULL.scremi_p55_actc%TYPE;
 VRESCRI_P55_ACTC CLR_DCEMVFULL.rescri_p55_actc%TYPE;

 -- Tablas EX8001, COPAE8001
 CURSOR Cr_EX8001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACTC,LONTAR_L02_ACTC,NUMTAR_P02_ACTC,
         CODPRO_P03_ACTC,IMPTRA_P04_ACTC,IMPCON_P05_ACTC,IDETRA_P11_ACTC,TIMLOC_P12_ACTC,
         CODPAI_P19_ACTC,PUNSER_P22_ACTC,CODFUN_P24_ACTC,CODRAZ_P25_ACTC,
         CODACT_P26_ACTC,SESION_P28_ACTC,REFADQ_P31_ACTC,IDEADQ_P32_ACTC,
         NUMAUT_P38_ACTC,CODACR_P39_ACTC,IDETER_P41_ACTC,IDEEST_P42_ACTC,
         NOMEST_P43_ACTC,LOCEST_P43_ACTC,PAIEST_P43_ACTC,TIPCU1_P46_ACTC,SIGCU1_P46_ACTC,
         IMPCU1_P46_ACTC,TIPCU2_P46_ACTC,SIGCU2_P46_ACTC,IMPCU2_P46_ACTC,
         TIPCU3_P46_ACTC,SIGCU3_P46_ACTC,IMPCU3_P46_ACTC,TIPCU4_P46_ACTC,
         SIGCU4_P46_ACTC,IMPCU4_P46_ACTC,CODR13_P48_ACTC,BINADQ_P48_ACTC,
         VTAPLA_P48_ACTC,FILLER_P48_ACTC,MONTRA_P49_ACTC,LONR50_P48_ACTC,
         CODR50_P48_ACTC,DIFERI_P48_ACTC,TIPMOV_P48_ACTC,MONCON_P50_ACTC,
         ORIIDE_P56_ACTC,ORITIM_P56_ACTC,NUMEMI_P95_ACTC,MCASHB_P48_ACTC,
         TIPTRA_P48_ACTC,INLOTE_P29_ACTC
         ,FILLER_P62_ACTC  as TransactionIdentifier
         -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACTC ,'YYMMDD'),'YYYY')) idparticion
          ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'YYYY'))||SUBSTR(sesion_p28_actc,3,2)||TO_CHAR(TO_DATE(sesion_p28_actc,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
         ,TO_NUMBER(TO_CHAR(TO_DATE(SESION_P28_ACTC ,'YYMMDD'),'MM')) id_mes
    FROM CLR_EX8001
   WHERE SESION_P28_ACTC = vFechac6
     AND SUBSTR(IDEADQ_P32_ACTC,3,4) = pBanco    ;

 CURSOR Cr_CLR_COPAE8001 IS
  SELECT ID_REGISTRO,IDEMEN_P00_ACCP,LONTAR_L02_ACCP,NUMTAR_P02_ACCP,
         IDETRA_P11_ACCP,TIMLOC_P12_ACCP,CODFUN_P24_ACCP,CODRAZ_P25_ACCP,
         SESION_P28_ACCP,INLOTE_P29_ACCP,IDEADQ_L32_ACCP,IDEADQ_P32_ACCP,
         IDEPRE_L33_ACCP,IDEPRE_P33_ACCP,CODACR_P39_ACCP,LONCTR_P46_ACCP,
         TIPCU1_P46_ACCP,SIGCU1_P46_ACCP,IMPCU1_P46_ACCP,TIPCU2_P46_ACCP,
         SIGCU2_P46_ACCP,IMPCU2_P46_ACCP,TIPCU3_P46_ACCP,SIGCU3_P46_ACCP,
         IMPCU3_P46_ACCP,TIPCU4_P46_ACCP,SIGCU4_P46_ACCP,IMPCU4_P46_ACCP,
         ORIDAT_L56_ACCP,ORIIDE_P56_ACCP,ORITRA_P56_ACCP,ORITIM_P56_ACCP,
         ORIADQ_L56_ACCP,ORIADQ_P56_ACCP,NUMMEN_P71_ACCP,REGDAT_L72_ACCP,
         REGDAT_P72_ACCP,TEXTO_L104_ACCP,TEXTO_P104_ACCP
         -- ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP ,'YYMMDD'),'YYYY')) idparticion /* Implementacion del particionamiento */
         ,SGCVNZ.FN_GETPARTICION(TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'YYYY'))||SUBSTR(SESION_P28_ACCP,3,2)||TO_CHAR(TO_DATE(SESION_P28_ACCP,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion
         ,TO_NUMBER(TO_CHAR(TO_DATE(SESION_P28_ACCP ,'YYMMDD'),'MM')) id_mes
    FROM CLR_COPAE8001
   WHERE SESION_P28_ACCP = vFechac6
     AND SUBSTR(IDEADQ_P32_ACCP,3,4) = pBanco
     AND 1 = 2;

 rCr_EX8001    Cr_EX8001%ROWTYPE;
 rCr_COPAE8001 Cr_CLR_COPAE8001%ROWTYPE;

 BEGIN

  SELECT Cod_EntAdq INTO vCod_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;

  nIdProceso:= PQMONPROC.InsMonProc('POUTVIRV'||vCod_Entadq);
  PQMONPROC.InsLog(nIdProceso,'M','Se inicia Proceso Outgoing Visa - ' || vCod_EntAdq);

  IF nvl(pBanco,0)= 0 THEN
     vSqlErrM := 'La entidad no puede ser Nulo!';
     RAISE r_error;
  END IF;

  vFechac  := pFecha;
    dbms_output.put_line('vFechac:'||vFechac);
  vFechac6 := SUBSTR(pFecha,3,6);
  vFechad  := to_date(vFechac,'YYYYMMDD');
  vFecha10 := to_date(substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4), 'DD/MM/YYYY');

  vCodMoneda:= PQCOMERCIOS.GCW_F_GETMONEDAVIG(vFecha10);

  dbms_output.put_line('vCodMoneda:'||vCodMoneda);

  SELECT C00NREDU INTO vDesc_EntAdq
    FROM Entidades_price
   WHERE C00LTIPO = 0
     AND C00LCSB = pBanco;


  dbms_output.put_line('vDesc_EntAdq:'||vDesc_EntAdq);

  vNroOperBol := f_NroOper(vFechad,vCodMoneda);
  dbms_output.put_line('vNroOperBol:'||vNroOperBol);
  vMtoNetoBol := f_MtoNeto(vFechad,vCodMoneda);
  dbms_output.put_line('vMtoNetoBol:'||vMtoNetoBol);
  p_Inicializa_Contadores(vFechac,vCodMoneda);

  vFileOutVi :='OUTVIRV'|| pBanco || vFechac||'.DAT' ;
  dbms_output.put_line('vFileOutVi:'||vFileOutVi);
  vFileOutVi1 :='OUTVIRV'|| pBanco || vFechac||'.TXT' ;
    dbms_output.put_line('vFileOutVi1:'||vFileOutVi1);
  PQMONPROC.inslog(nIdProceso,'M','Fecha de Proceso: ' || substr(pFecha,7,2) || '/' || substr(pFecha,5,2) || '/' || substr(pFecha,1,4));
  vC :=pqOutput.open_file(vFileOutVi,kDirOut_Alias);
    dbms_output.put_line('vC:'||vC);

  IF vC <>'OK' THEN
     pR_CodRes:='9003';
     pR_DesRes:='ERR-APL ARCHIVO '||RPAD(vFileOutVi,25,' ')||' NO SE PUEDE ABRIR';
      dbms_output.put_line('****************************************************************************************');
      dbms_output.put_line('**** ' || pR_DesRes || ' .... ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO VISANET ');
      dbms_output.put_line('****************************************************************************************');
     RAISE r_error;
  ELSE
     PQMONPROC.InsLog(nIdProceso,'M','Se inicia Generacion de Outgoing Visa: ' || vFileOutVi || ' a las ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
      dbms_output.put_line('**** INICIA  GENERACION DE OUTGOING VISA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
      dbms_output.put_line('****************************************************************************');
  END IF;

  file_id1 := utl_file.FOPEN( kDirOUT_Alias2, vFileOutVi1, 'W' );

  OPEN Cr_EX8001;
     dbms_output.put_line('**** ABRIENDO CURSOR EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     PQMONPROC.InsLog(nIdProceso,'M','Abriendo Cursor EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
     -- Se carga primero los registros Rechazados
     p_CargaRegRechazados_ventas_r(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
     BEGIN

     FETCH Cr_EX8001 INTO  rCr_EX8001;
      EXIT WHEN Cr_EX8001%NOTFOUND;
           IF rCr_EX8001.CODPRO_P03_ACTC IN ('000000','010000','200000') AND
              (rCr_EX8001.IDEMEN_P00_ACTC = '1240' OR (rCr_EX8001.IDEMEN_P00_ACTC = '1440' AND rCr_EX8001.REFADQ_P31_ACTC is not null)) THEN
            /*  IF SUBSTR(rCr_EX8001.PUNSER_P22_ACTC, 7, 1) IS NOT NULL THEN
                 cont := cont+1;
              END IF;    */
              vTCR_0.Trn_Code := f_Trn_Code_rev(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC, rCr_EX8001.ORIIDE_P56_ACTC);
                  dbms_output.put_line('vTCR_0.Trn_Code:'||vTCR_0.Trn_Code);
              vTCR_0.Trn_Code_Qlf := '0';
              vTCR_0.Trn_Comp_Seq_Num := '0';
              vTCR_0.Account_Num := rpad(SUBSTR(rCr_EX8001.NUMTAR_P02_ACTC,1,19),19,'0');
                                                dbms_output.put_line('vTCR_0.Account_Num:'||vTCR_0.Account_Num);
              vTCR_0.Floor_Limit_Ind:=' ';
              vTCR_0.CRB_Excep_File_Ind := ' ';
              vTCR_0.PCAS_ind := ' ';
              vTCR_0.Acq_Ref_Num := rCr_EX8001.REFADQ_P31_ACTC;
                                  dbms_output.put_line('vTCR_0.Acq_Ref_Num:'||vTCR_0.Acq_Ref_Num);
              vTCR_0.Acq_Bus_ID := LPAD('0',8,'0'); --'00000000';
              IF (rCr_EX8001.ORITIM_P56_ACTC IS NULL) OR (rCr_EX8001.ORITIM_P56_ACTC = '000000000000') THEN
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,3,4);
              ELSE
                 vTCR_0.Purchase_date := SUBSTR(rCr_EX8001.ORITIM_P56_ACTC,3,4);
              END IF;
              vTCR_0.Dest_Amount := LPAD('0',12,'0'); --'000000000000';
              vTCR_0.Dest_Curren_Code := LPAD(' ',3,' '); --'   ';
              vTCR_0.Source_Amount :=  f_Source_Amount(rCr_EX8001.impcon_p05_actc ,rCr_EX8001.imptra_p04_actc);
              vTCR_0.Source_Curren_Code := f_Source_Currency_Code(rCr_EX8001.impcon_p05_actc ,rCr_EX8001.moncon_p50_actc ,rCr_EX8001.montra_p49_actc);
              IF ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)) >= '3000' AND ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)) <= '3299' THEN
                 vTCR_0.Merch_Name := f_buscap26(rCr_EX8001.CODACT_P26_ACTC) || '             ';
              ELSE
                 vTCR_0.Merch_Name := RPAD(ltrim(rtrim(rCr_EX8001.NOMEST_P43_ACTC)),25,' ');
              END IF;
              vTCR_0.Merch_city := NVL(RPAD(ltrim(rtrim(rCr_EX8001.LOCEST_P43_ACTC)),13,' '),'             ');
               vTCR_0.Merch_Count_Code := RPAD(ltrim(rtrim(rCr_EX8001.PAIEST_P43_ACTC)),3,' ');
              vTCR_0.Merch_Cate_Code  := NVL(ltrim(rtrim(rCr_EX8001.CODACT_P26_ACTC)),'    ');
              vTCR_0.Merch_Zip_Code   := LPAD('0',5,'0'); --'00000';
              vTCR_0.Merch_Prov_Code  := LPAD(' ',3,' '); --'   ';
              vTCR_0.Req_Payment_Serv :=' ';
              vTCR_0.Reserved_01 := ' ' ;
              vTCR_0.Usage_Code := f_Usage_Code_05(rCr_EX8001.codfun_p24_actc);
              vTCR_0.Reason_Code := f_Reason_Code(rCr_EX8001.CODRAZ_P25_ACTC,rCr_EX8001.codfun_p24_actc);
              vTCR_0.Settlement_Flag  := f_settlement_flag(rCr_EX8001.BINADQ_P48_ACTC, rCr_EX8001.INLOTE_P29_ACTC);
                vTCR_0.Aut_Charact_Ind := ' '; --f_Aut_Charact_Ind(vTCR_0.Trn_Code, SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,5,1) ,SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,6,1));
              IF rCr_EX8001.NUMAUT_P38_ACTC is not null THEN
                 vTCR_0.Authorization_Code := rpad(LTRIM(RTRIM(rCr_EX8001.NUMAUT_P38_ACTC)),6,' ');
              ELSE
                 vTCR_0.Authorization_Code := lpad(' ',6,' ');
              END IF;
              vTCR_0.POS_Capability  := f_Pos_Terminal_Capability(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,1,1));
               vTCR_0.Cardholder_ID_Met := f_Cardholder_IDMethod(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,8,1));
              vTCR_0.Collec_Only_Flag :=' ';
              vTCR_0.POS_Entry_Mode := f_POS_EntryMode(SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,7,1),SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,5,1));
              vTCR_0.Inter_Fee_Ind := f_international_fee_indicator(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode);
              IF vTCR_0.Trn_Code = '25' THEN
                 vTCR_0.Central_Proc_Date := f_JulianDate(SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,1,6));
              ELSE
                 vTCR_0.Central_Proc_Date := '0000'; --f_JulianDate(SUBSTR(rCr_EX8001.TIMLOC_P12_ACTC,1,6));
              END IF;
              vTCR_0.Reimb_Attribute := f_reimbursement_attribute(vTCR_0.Settlement_Flag, vTCR_0.POS_Entry_Mode, rCr_EX8001.INLOTE_P29_ACTC, vTCR_0.Trn_Code);

              linea := LPAD(vTCR_0.Trn_Code,2,'0')|| vTCR_0.Trn_Code_Qlf || vTCR_0.Trn_Comp_Seq_Num ||
                       vTCR_0.Account_Num || vTCR_0.Floor_Limit_Ind ||vTCR_0.CRB_Excep_File_Ind  ||
                       vTCR_0.PCAS_ind || vTCR_0.Acq_Ref_Num ||lpad(vTCR_0.Acq_Bus_ID,8,'0') ||
                       vTCR_0.Purchase_date || LPAD(vTCR_0.Dest_Amount,12,'0') ||vTCR_0.Dest_Curren_Code ||
                       LPAD(to_char(vTCR_0.Source_Amount),12,'0') || vTCR_0.Source_Curren_Code ||
                       vTCR_0.Merch_Name || vTCR_0.Merch_city || vTCR_0.Merch_Count_Code ||
                       vTCR_0.Merch_Cate_Code || vTCR_0.Merch_Zip_Code || vTCR_0.Merch_Prov_Code ||
                       vTCR_0.Req_Payment_Serv || vTCR_0.Reserved_01 || vTCR_0.Usage_Code ||
                       lpad(vTCR_0.Reason_Code,2,'0') || vTCR_0.Settlement_Flag || vTCR_0.Aut_Charact_Ind ||
                       vTCR_0.Authorization_Code || vTCR_0.POS_Capability || vTCR_0.Inter_Fee_Ind ||
                       vTCR_0.Cardholder_ID_Met || vTCR_0.Collec_Only_Flag || vTCR_0.POS_Entry_Mode ||
                       vTCR_0.Central_Proc_Date || vTCR_0.Reimb_Attribute;
                    dbms_output.put_line('lineea1:'||linea);
              pqOutput.PUT_LINE(linea);
          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              p_Update_Contadores(SUBSTR(vTCR_0.Acq_Ref_Num,2,6), vtcr_0.Trn_Code, vtcr_0.Usage_Code, vtcr_0.Source_Curren_Code, vtcr_0.Settlement_Flag, vtcr_0.Source_Amount, rCr_EX8001.IMPCU1_P46_ACTC);
          --  Contadores para TC1
              vNum_Tcrs_tc91:= vNum_Tcrs_tc91+1;
              vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_0.Source_Amount;
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;
              vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_0.Source_Amount;

  -------------------------------------------------TCR1----------------------------------

              vCtaSelec := ' ';
              IF SUBSTR(rCr_EX8001.FILLER_P48_ACTC, 6, 3) = vCodMoneda THEN
                 vCtaSelec := '0';
              END IF;
              IF SUBSTR(rCr_EX8001.FILLER_P48_ACTC, 6, 3) = '840' THEN
                 vCtaSelec := '1';
              END IF;

              vTCR_1.Trn_Code := f_Trn_Code_rev(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC, rCr_EX8001.ORIIDE_P56_ACTC);
              vTCR_1.Trn_Code_Qlf :='0';
              vTCR_1.Trn_Comp_Seq_Num :='1';
              vTCR_1.Iss_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Acq_Workstat_BIN := LPAD(' ',6,' '); --'      ';
              vTCR_1.Chargeback_Ref_Num := f_Chrgback_Ref_Num(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.NUMEMI_P95_ACTC);
              --vTCR_1.Documentation_Ind := f_Documentation_Indicator(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.CODRAZ_P25_ACTC);
              p_Documentation_indicator(rCr_EX8001.CODFUN_P24_ACTC,rCr_EX8001.refadq_p31_actc, nIdProceso, vTCR_1.Documentation_Ind);
              vTCR_1.Member_Mess_Text := f_member_message_text(rCr_EX8001.CODFUN_P24_ACTC, rCr_EX8001.NUMEMI_P95_ACTC, rCr_EX8001.LONR50_P48_ACTC, rCr_EX8001.CODR50_P48_ACTC, rCr_EX8001.VTAPLA_P48_ACTC, rCr_EX8001.DIFERI_P48_ACTC);
              vTCR_1.Special_Cond_Ind := LPAD(' ',2,' '); --'  ';
              vTCR_1.FeeProgramID := LPAD(' ',3,' '); --'  '
              vTCR_1.Issuer_charge  := LPAD(' ',1,' ');
              vTCR_1.Reserved_01 := LPAD(' ',1,' '); --'     ';
              vTCR_1.Card_Acceptor_ID := rpad(rCr_EX8001.IDEEST_P42_ACTC, 15, ' ');--rpad((substr(rCr_EX8001.IDEEST_P42_ACTC,3,3) || substr(rCr_EX8001.IDEEST_P42_ACTC,10,6)) || vCtaSelec, 15, ' ');
              vTCR_1.Terminal_ID := f_Terminal_ID(rCr_EX8001.PUNSER_P22_ACTC, rCr_EX8001.IDETER_P41_ACTC, rCr_EX8001.REFADQ_P31_ACTC, rCr_EX8001.IDEADQ_P32_ACTC);
              --vTCR_1.Terminal_ID := rCr_EX8001.IDETER_P41_ACTC;--Cambio pedido de AV 06 SEP 2004
              vTCR_1.National_Reimb_Fee := f_Nat_Reimbursement_fee_06(VTCR_0.Settlement_Flag,rCr_EX8001.IMPCU1_P46_ACTC);
              vTCR_1.Mail_Phone_Ind := f_mail_telephon_indicator_06(rCr_EX8001.PUNSER_P22_ACTC,rCr_EX8001.CODACT_P26_ACTC);
              vTCR_1.Spec_Chargeback_Ind := ' ';
              vTCR_1.Interface_Trace_Num := LPAD('0',6,'0'); --'000000';
              --vTCR_1.Cardhold_ActTer_Ind := ' '; --comentado pedido AV
              vTCR_1.Cardhold_ActTer_Ind := f_Cardhold_ActTer_Ind(rCr_EX8001.IDETER_P41_ACTC, rCr_EX8001.IDEADQ_P32_ACTC, rCr_EX8001.TIPMOV_P48_ACTC);
              vTCR_1.Prepaid_Card_Ind := ' ';
              vTCR_1.Reserved_02 := ' ';
              vTCR_1.AVS_Response_Code := ' ';
              vTCR_1.Aut_Source_Code := ' ';
              vTCR_1.Purchase_Iden_Form := ' ';
              vTCR_1.ATM_Account_Select := '0';
              vTCR_1.Inst_Payment_Count := '  ';
              vTCR_1.Purchase_Identifier := LPAD(' ',25,' '); --'                         ';
              varCashback := NVL(LPAD(rCr_EX8001.MCASHB_P48_ACTC, 9, '0'), '000000000');
              vTCR_1.Chip_Cond_Code := f_Chip_Cond_Code(rCr_EX8001.PUNSER_P22_ACTC);
              vTCR_1.POS_Environment  := f_POS_Environment(rCr_EX8001.PUNSER_P22_ACTC);

              linea := LPAD(vTCR_1.Trn_Code,2,'0') ||   LPAD(vTCR_1.Trn_Code_Qlf,1,'0') ||  vTCR_1.Trn_Comp_Seq_Num ||
                       vTCR_1.Iss_Workstat_BIN || vTCR_1.Acq_Workstat_BIN ||   lpad(vTCR_1.Chargeback_Ref_Num,6,'0') ||
                       vTCR_1.Documentation_Ind || rpad(vTCR_1.Member_Mess_Text,50,' ') ||   vTCR_1.Special_Cond_Ind ||
                       vTCR_1.FeeProgramID    ||   vTCR_1.Issuer_charge ||
                       vTCR_1.Reserved_01 ||  vTCR_1.Card_Acceptor_ID ||   rpad(vTCR_1.Terminal_ID,8,' ') ||
                       lpad(vTCR_1.National_Reimb_Fee,12,'0') ||  vTCR_1.Mail_Phone_Ind ||   vTCR_1.Spec_Chargeback_Ind ||
                       lpad(vTCR_1.Interface_Trace_Num,6,'0') ||  vTCR_1.Cardhold_ActTer_Ind ||   vTCR_1.Prepaid_Card_Ind ||
                       vTCR_1.Reserved_02 ||  vTCR_1.AVS_Response_Code ||   vTCR_1.Aut_Source_Code ||
                       vTCR_1.Purchase_Iden_Form || vTCR_1.ATM_Account_Select ||  vTCR_1.Inst_Payment_Count ||
                       vTCR_1.Purchase_Identifier || varCashback ||  vTCR_1.Chip_Cond_Code ||  vTCR_1.POS_Environment;
                    dbms_output.put_line('lineea2:'||linea);
              pqOutput.PUT_LINE(linea);

          --  GENERACION DE OUTVIEEEEYYYYMMDD.TXT
              IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                 utl_file.PUT_LINE( file_id1, linea);
              ELSE
                 IF vTCR_0.Trn_Code in ('06','25') THEN
                    utl_file.PUT_LINE( file_id1, linea);
                 END IF;
              END IF;
          --  Contadores para tc91 y 92 del Tc07
              vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's (Se agrega)
              vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's (Se agrega)

-----------------
--vTCR_5 y vTCR_7
-----------------

              IF SUBSTR(rCr_EX8001.PUNSER_P22_ACTC,7,1)='5' THEN

                 SELECT COUNT(1)
                   INTO VCUENTA
                   FROM CLR_DCEMVFULL
                  WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX8001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                    AND IDETRA_P11_ACTC = rCr_EX8001.IDETRA_P11_ACTC
                    AND TIMLOC_P12_ACTC = rCr_EX8001.TIMLOC_P12_ACTC
                    AND IDEADQ_P32_ACTC = rCr_EX8001.IDEADQ_P32_ACTC
                    AND  idparticion             =rCr_EX8001.idparticion   /* Implementaci¿n del particionamiento TST 19/04/2014 */
                    AND  id_mes                 =rCr_EX8001.id_mes ;     /* Implementaci¿n del particionamiento TST 19/04/2014 */


                 IF VCUENTA > 0  THEN

                    SELECT TTCRIP_P55_ACTC,NUMSEC_P23_ACTC,FECCRI_P55_ACTC,CAPTER_P55_ACTC,
                           PAICRI_P55_ACTC,NUMALE_P55_ACTC,EMVATC_P55_ACTC,PERINT_P55_ACTC,
                           CRIPPE_P55_ACTC,DAPLEM_P55_ACTC,EMVTVR_P55_ACTC,IMPCRI_P55_ACTC,
                           SCREMI_P55_ACTC, RESCRI_P55_ACTC
                      INTO VTTCRIP_P55_ACTC,VNUMSEC_P23_ACTC,VFECCRI_P55_ACTC,VCAPTER_P55_ACTC,
                           VPAICRI_P55_ACTC,VNUMALE_P55_ACTC,VEMVATC_P55_ACTC,VPERINT_P55_ACTC,
                           VCRIPPE_P55_ACTC,VDAPLEM_P55_ACTC,VEMVTVR_P55_ACTC,VIMPCRI_P55_ACTC,
                           VSCREMI_P55_ACTC,VRESCRI_P55_ACTC
                      FROM CLR_DCEMVFULL
                     WHERE IDEMEN_P00_ACTC = DECODE(rCr_EX8001.IDEMEN_P00_ACTC,'1240','1244','1440','1444')
                       AND IDETRA_P11_ACTC = rCr_EX8001.IDETRA_P11_ACTC
                       AND TIMLOC_P12_ACTC = rCr_EX8001.TIMLOC_P12_ACTC
                       AND IDEADQ_P32_ACTC = rCr_EX8001.IDEADQ_P32_ACTC
                       AND  idparticion             =rCr_EX8001.idparticion   /* Implementaci¿n del particionamiento TST 19/04/2014 */
                       AND  id_mes                 =rCr_EX8001.id_mes;  /* Implementaci¿n del particionamiento TST 19/04/2014 */


                    vTCR_5.Trn_Code                       := f_Trn_Code_rev(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC,rCr_EX8001.ORIIDE_P56_ACTC);       --   1-  2   Transaction Code --LPAD('0',2,'0');
                    vTCR_5.Trn_Code_Qlf                   := '0';
                    vTCR_5.Trn_Comp_Seq_Num               := '5';
                    vTCR_5.TransactionIdentifier          :=  NVL(substr(rCr_EX8001.TransactionIdentifier,1,15), LPAD('0',15,'0')); --  rCr_EX8001.TransactionIdentifier ;-- carta tecnica visa 2013 inclusin del TID MODIFICADO POR FELIPE VITALE
                     dbms_output.put_line('ENTRO 4: '|| vTCR_5.TransactionIdentifier );
                    vTCR_5.AuthorizedAmount               := LPAD(TO_CHAR(rCr_EX8001.IMPTRA_P04_ACTC),12,'0');  --LPAD('0',12,'0');
                    vTCR_5.AuthorizationCurrencyCode      := LPAD(rCr_EX8001.MONTRA_P49_ACTC,3,' ');  --pSource_Curren_Code; --LPAD(' ',3,' ');
                    --dbms_output.put_line('vTCR_5.AuthorizationCurrencyCode:'||vTCR_5.AuthorizationCurrencyCode);
                    vTCR_5.AuthorizationResponseCode      := LPAD(NVL(TRIM(VRESCRI_P55_ACTC),0),2,'0'); --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0');
                    vTCR_5.ValidationCode                 := LPAD(' ',4,' ');
                    vTCR_5.ExcludedTranIdenReason         := LPAD(' ',1,' ');
                    vTCR_5.CRSProcessingCode              := LPAD(' ',1,' ');
                    vTCR_5.ChargebackRightsIndicator      := LPAD(' ',2,' ');
                    vTCR_5.MultipleClearingSequenceNumber := LPAD('0',2,'0');
                    vTCR_5.MultipleClearingSequenceCount  := LPAD('0',2,'0');
                    vTCR_5.MarketSpecificAutDataInd       := LPAD(' ',1,' ');
                    vTCR_5.TotalAuthorizedAmount          := LPAD('0',12,'0');
                    vTCR_5.InformationIndicator           := LPAD(' ',1,' ');
                    vTCR_5.MerchantTelephoneNumber        := LPAD(' ',14,' ');
                    vTCR_5.AdditionalDataIndicator        := LPAD(' ',1,' ');
                    vTCR_5.MerchantVolumeIndicator        := LPAD(' ',2,' ');
                    vTCR_5.ElectronicCommGoodsInd         := LPAD(' ',2,' ');
                    vTCR_5.MerchantVerificationValue      := LPAD(' ',10,' ');
                    vTCR_5.InterchangeFeeAmount           := LPAD('0',15,'0');
                    vTCR_5.InterchangeFeeSign             := LPAD(' ',1,' ');
                    vTCR_5.SourceCurrBaseCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.BaseCurrtoDestCurrExchRate     := LPAD('0',8,'0');
                    vTCR_5.OptionalIssuerISAAmount        := LPAD('0',12,'0');
                    vTCR_5.ProductID                      := LPAD(' ',2,' ');
                    vTCR_5.ProgramID                      := LPAD(' ',6,' ');
                    vTCR_5.Reserved                       := LPAD(' ',24,' ');
                    vTCR_5.CVV2ResultCode                 := LPAD(' ',1,' ');


                     IF TRIM(vTCR_5.ProductID) = 'I' THEN
                        vTCR_5.ProductID := 'I1';
                    END IF;

                    linea := LPAD(vTCR_5.Trn_Code,2,'0')||vTCR_5.Trn_Code_Qlf||
                             vTCR_5.Trn_Comp_Seq_Num||vTCR_5.TransactionIdentifier||
                             vTCR_5.AuthorizedAmount||vTCR_5.AuthorizationCurrencyCode||
                             vTCR_5.AuthorizationResponseCode||vTCR_5.ValidationCode||
                             vTCR_5.ExcludedTranIdenReason||vTCR_5.CRSProcessingCode||
                             vTCR_5.ChargebackRightsIndicator||vTCR_5.MultipleClearingSequenceNumber||
                             vTCR_5.MultipleClearingSequenceCount||vTCR_5.MarketSpecificAutDataInd||
                             vTCR_5.TotalAuthorizedAmount||vTCR_5.InformationIndicator||
                             vTCR_5.MerchantTelephoneNumber||vTCR_5.AdditionalDataIndicator||
                             vTCR_5.MerchantVolumeIndicator||vTCR_5.ElectronicCommGoodsInd||
                             vTCR_5.MerchantVerificationValue||vTCR_5.InterchangeFeeAmount||
                             vTCR_5.InterchangeFeeSign||vTCR_5.SourceCurrBaseCurrExchRate||
                             vTCR_5.BaseCurrtoDestCurrExchRate||vTCR_5.OptionalIssuerISAAmount||
                             vTCR_5.ProductID||vTCR_5.ProgramID||
                             vTCR_5.Reserved||
                             vTCR_5.CVV2ResultCode;
                                                   -- BSP Angel Campoli 21-08-2013;;
                    --dbms_output.put_line('ENTRO 4 det ; ' || linea);

                    pqOutput.PUT_LINE(linea);

                    --Contadores para tc91 y 92 del Tc05
                    vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;
                    vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;


                    vTCR_7.Trn_Code := f_Trn_Code_rev(rCr_EX8001.IDEMEN_P00_ACTC,rCr_EX8001.CODPRO_P03_ACTC,rCr_EX8001.ORIIDE_P56_ACTC);
                    vTCR_7.Trn_Code_Qlf := '0';
                    vTCR_7.Trn_Comp_Seq_Num := '7';
                    vTCR_7.dctcttcrip := LPAD(NVL(VTTCRIP_P55_ACTC,'0'),2,'0');
                    vTCR_7.dcnumsec := LPAD(NVL(VNUMSEC_P23_ACTC,0),3,'0');
                    vTCR_7.dctcfeccri := LPAD(VFECCRI_P55_ACTC,6,'0');
                    vTCR_7.dctccapter := LPAD(VCAPTER_P55_ACTC,6,'0');
                    vTCR_7.dctcpaicri := '862'; -- RPAD(VPAICRI_P55_ACTC,3,' ');
                    vTCR_7.Num_serie := LPAD (' ',8,' '); --LPAD(rCr_EX8001.IDETER_P41_ACTC,8,' '); --' ';
                    vTCR_7.dctcnumale := LPAD(VNUMALE_P55_ACTC,8,'0');
                    vTCR_7.dctcemvatc := LPAD(VEMVATC_P55_ACTC,4,'0');
                    vTCR_7.dctcperint := LPAD(VPERINT_P55_ACTC,4,'0');
                    vTCR_7.dctccrippe := LPAD(VCRIPPE_P55_ACTC,16,'0');
                    vTCR_7.dctcdaplem01 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),3,2),2,'0');
                    vTCR_7.dctcdaplem02 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),5,2),2,'0');
                    vTCR_7.dctcemvtvr := LPAD(VEMVTVR_P55_ACTC,10,'0');
                    vTCR_7.dctcdaplem03 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),7,8),8,'0');
                    vTCR_7.dctcimpcri := LPAD(VIMPCRI_P55_ACTC,12,'0');
                    vTCR_7.dctcdaplem04 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),15,2),2,'0');
                    vTCR_7.dctcdaplem05 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),17,16),16,'0');
                    vTCR_7.dctcdaplem06 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),1,2),2,'0');
                    vTCR_7.dctcdaplem07 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),33,2),2,'0');
                    vTCR_7.dctcdaplem08 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),35,30),30,'0');
                    vTCR_7.Espacios := lpad(' ',8,' ');
                    vTCR_7.dctcscremi := nvl(LPAD(STD.F_RTRIM4HEX(VSCREMI_P55_ACTC),10,'0'),kSCREMI_P55_ACTC_DEF);

                    linea := LPAD(vTCR_7.Trn_Code,2,'0') || vTCR_7.Trn_Code_Qlf || vTCR_7.Trn_Comp_Seq_Num ||
                             vTCR_7.dctcttcrip || LPAD(vTCR_7.dcnumsec,3,'0')|| vTCR_7.dctcfeccri||vTCR_7.dctccapter||
                             vTCR_7.dctcpaicri||vTCR_7.Num_serie||vTCR_7.dctcnumale||vTCR_7.dctcemvatc||
                             vTCR_7.dctcperint||vTCR_7.dctccrippe||vTCR_7.dctcdaplem01||vTCR_7.dctcdaplem02||
                             vTCR_7.dctcemvtvr||vTCR_7.dctcdaplem03||vTCR_7.dctcimpcri||vTCR_7.dctcdaplem04||
                             vTCR_7.dctcdaplem05||vTCR_7.dctcdaplem06||vTCR_7.dctcdaplem07||vTCR_7.dctcdaplem08||
                             vTCR_7.Espacios||vTCR_7.dctcscremi;

                    dbms_output.put_line('linea:' || linea);
                    pqOutput.PUT_LINE(linea);

                     --  Contadores para tc91 y 92 del Tc07
                     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
                     vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's
                 ELSE
                    IF vTCR_0.Trn_Code = '05' then
                       dbms_output.put_line('Alerta TCR_0 CHIP NO CARGADA:  ' || rCr_EX8001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                       PQMONPROC.InsLog(nIdProceso,'W','Alerta TCR_0 CHIP NO CARGADA: ' || rCr_EX8001.REFADQ_P31_ACTC || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
                    END IF;
                 END IF;


              END IF;

          --  FIN DE GENERACION DE OUTVIEEEEYYYYMMDD.TXT
          --  **********Para el TC91************
              vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
              vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
              vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
              vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
              IF vNum_MTrans_tc91 = 500 THEN
                 vNum_Tcrs_tc91 := vNum_Tcrs_tc91+1;--Numero de TCR's..91
                 vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's..92
                 vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                 vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                 vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                 vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                 linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                          vBatch_Number_tc91,vNum_Tcrs_tc91,
                          vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                 pqOutput.PUT_LINE(linea);
                 vNum_MTrans_tc91 := 0;
                 vNum_Tcrs_tc91 := 0;
                 vNum_Trns_tc91 := 0;
                 vSource_Amount_tc91 := 0;
              END IF;
           END IF;
  EXCEPTION
     WHEN OTHERS THEN
          vSQLError:=SQLCODE;
          vSqlErrM :=SUBSTR(SQLERRM,1,200);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR EX8001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.InsLog(nIdProceso, 'E', 'INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA','99');
            dbms_output.put_line('******************************************************************************************');
             dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
             dbms_output.put_line('**** PROCESANDO CURSOR EX8001 - ' || vDesc_EntAdq );
             dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA');
             dbms_output.put_line('******************************************************************************************');
          RETURN;
     END;
  END LOOP;
  dbms_output.put_line('**** CERRANDO CURSOR EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor EX8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));

  CLOSE Cr_EX8001;

  -- Para el TC91

  IF vNum_Tcrs_tc91 >= 0 THEN
 -- las sgtes 4 lineas han sido descomentadas EL 04 MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91, vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     pqOutput.PUT_LINE(linea);
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:=0;
  END If;
-- Fin para TC91

  OPEN Cr_CLR_COPAE8001;
  dbms_output.put_line('**** ABRIENDO CURSOR COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       PQMONPROC.inslog(nIdProceso,'M','Abriendo Cursor COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
       p_CargaRegRechazados_rev(vCod_EntAdq,vFechac6,file_id1,vNum_Tcrs_tc91,vSource_Amount_tc91,vNum_Tcrs_tc92,vSource_Amount_tc92,vNum_MTrans_tc91,vNum_Trns_tc91,vNum_MTrans_tc92,vNum_Trns_tc92,vBatch_Number_tc91,vBatch_Number_tc92);
  LOOP
   BEGIN
      FETCH Cr_CLR_COPAE8001 INTO  rCr_COPAE8001;
      EXIT WHEN Cr_CLR_COPAE8001%NOTFOUND;
         IF rCr_COPAE8001.IDEMEN_P00_ACCP = '1740'  THEN
            vTCR_10_20.Trn_Code := f_tc10_20_Trn_Code_rev(rCr_COPAE8001.SIGCU1_P46_ACCP);
            vTCR_10_20.Trn_Code_Qlf:= '0';
            vTCR_10_20.Trn_Comp_Seq_Num:= '0';
            vTCR_10_20.Destination_BIN := '000000';
            IF SUBSTR(rCr_COPAE8001.IDEADQ_P32_ACCP,3,4) ='0105' THEN
               vTCR_10_20.Source_BIN := '411032';
            ELSIF SUBSTR(rCr_COPAE8001.IDEADQ_P32_ACCP,3,4) ='0108' THEN
               vTCR_10_20.Source_BIN := '420198';
            END IF;
            vTCR_10_20.Reason_Code := f_tc10_Reason_Code(rCr_COPAE8001.CODRAZ_P25_ACCP);
            vTCR_10_20.Country_Code := f_Country_Code(vTCR_10_20.Reason_Code);
               vTCR_10_20.Event_Date:= SUBSTR(rCr_COPAE8001.TIMLOC_P12_ACCP,3,4);
            vTCR_10_20.Account_Num := rpad(ltrim(rtrim(rCr_COPAE8001.numtar_p02_accp)),19,'0');
            IF (ltrim(rtrim(rCr_COPAE8001.numtar_p02_accp)) = '' OR TO_NUMBER(rCr_COPAE8001.numtar_p02_accp)=0 OR rCr_COPAE8001.numtar_p02_accp is null) AND
               (rCr_COPAE8001.codraz_p25_accp = '7703') THEN
               IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0002' THEN
                    vTCR_10_20.Destination_BIN := '450645';
                ELSE
                   IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0003' THEN
                      vTCR_10_20.Destination_BIN := '454775';
                  ELSE
                     IF substr(rCr_COPAE8001.regdat_p72_accp,12,4) ='0011' THEN
                         vTCR_10_20.Destination_BIN := '492112';
                     END IF;
                  END IF;
               END IF;
            END IF;
            vTCR_10_20.Dest_Amount:= 0;
            vTCR_10_20.Dest_Curren_Code := '   ';
            IF rCr_COPAE8001.IMPCU1_P46_ACCP is null THEN
               vTCR_10_20.Source_Amount := 0;
            ELSE
               vTCR_10_20.Source_Amount := rCr_COPAE8001.IMPCU1_P46_ACCP;
            END IF;
            vTCR_10_20.Source_Curren_Code:= vCodMoneda;
            vTCR_10_20.Mess_Text:= RPAD(rCr_COPAE8001.TEXTO_P104_ACCP,70,' ');
            vTCR_10_20.Settlement_Flag := f_tc10_Settlement_flag(rCr_COPAE8001.INLOTE_P29_ACCP);--vTCR_10_20.Source_BIN, rCr_COPAE8001.TIPCU1_P46_ACCP);
            IF SUBSTR(rCr_COPAE8001.numtar_p02_accp,1,16) = '4506450000000016' and rCr_COPAE8001.codraz_p25_accp='7711' AND substr(rCr_COPAE8001.texto_p104_accp,1,14) = 'SECURITIZACION' THEN
               vTCR_10_20.Settlement_Flag := '0';
            END IF;
            vTCR_10_20.Trans_Identifier:= LPAD('0',15,'0');  --TID PENDIENTE
                      vTCR_10_20.Reserved_01:= ' ';
            vTCR_10_20.Central_Proc_Date:= '0000';
            vTCR_10_20.Reimb_Attribute:='0';
            linea := vTCR_10_20.Trn_Code ||  vTCR_10_20.Trn_Code_Qlf ||   vTCR_10_20.Trn_Comp_Seq_Num||
                     lpad(vTCR_10_20.Destination_BIN,6,'0') || lpad(vTCR_10_20.Source_BIN,6,'0') ||   lpad(vTCR_10_20.Reason_Code,4,'0') ||
                     vTCR_10_20.Country_Code || lpad(vTCR_10_20.Event_Date,4,'0') ||   vTCR_10_20.Account_Num ||
                     lpad(vTCR_10_20.Dest_Amount,12,'0') || vTCR_10_20.Dest_Curren_Code ||
                     lpad(vTCR_10_20.Source_Amount,12,'0') || vTCR_10_20.Source_Curren_Code || vTCR_10_20.Mess_Text ||
                     vTCR_10_20.Settlement_Flag || LPAD(vTCR_10_20.Trans_Identifier,15,'0') ||  vTCR_10_20.Reserved_01 ||
                     vTCR_10_20.Central_Proc_Date || vTCR_10_20.Reimb_Attribute;
             pqOutput.PUT_LINE(linea);
             utl_file.PUT_LINE( file_id1, linea);
             p_Update_Contadores(vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, '1', vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.Source_Amount, 0);
             p_Inserta_10_20(vFechac, vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, rCr_COPAE8001.CODRAZ_P25_ACCP, vTCR_10_20.Source_Amount);
             --**********Para el TC91************
             vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_10_20.Source_Amount;
             vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_10_20.Source_Amount;
             vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
             vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
             vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
             vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's
             vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
             vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
             IF vNum_MTrans_tc91 = 500 THEN
                vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
                vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
                vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                         vBatch_Number_tc91,vNum_Tcrs_tc91,
                         vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
                pqOutput.PUT_LINE(linea);
                vNum_MTrans_tc91 := 0;
                vNum_Tcrs_tc91 :=0;
                vNum_Trns_tc91 := 0;
                vSource_Amount_tc91:= 0;
             END IF;
          END IF;

   EXCEPTION
      WHEN OTHERS THEN
           vSQLError:=SQLCODE;
           vSqlErrM :=SUBSTR(SQLERRM,1,200);
           PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE8001 - ' || vDesc_EntAdq);
           vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
           PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
              dbms_output.put_line('******************************************************************************************');
              dbms_output.put_line('**** ERROR ORA: ' || vSQLError || ' - ' || vSqlErrM);
              dbms_output.put_line('**** PROCESANDO CURSOR COPAE8001 - Banco: ' || vDesc_EntAdq);
              dbms_output.put_line('**** INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ');
              dbms_output.put_line('******************************************************************************************');
           RETURN;
   END;

  END LOOP;
  PQMONPROC.inslog(nIdProceso,'M','Cerrando Cursor COPAE8001 - ' || vDesc_EntAdq || ' - '|| TO_CHAR(SYSDATE, 'HH:MM:SS'));
  CLOSE Cr_CLR_COPAE8001;
  -- Para el TC91
  IF vNum_Tcrs_tc91 >=0 THEN
     -- LAS SGTES 4 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
     vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
     vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
     vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
     vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
     vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
     vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
     linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                            vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,vFechac6) ;
     vNum_MTrans_tc91 := 0;
     vNum_Tcrs_tc91 :=0;
     vNum_Trns_tc91 := 0;
     vSource_Amount_tc91:= 0;
     pqOutput.PUT_LINE(linea);
  END IF;
   -- Para el TC92
   -- LAS SGTES 2 LINEAS HAN SIDO DESCOMENTADAS EL 04/MARZO/2008
  vNum_Tcrs_tc92 := vNum_Tcrs_tc92 + 1;
  vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
  vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
  linea := f_gen_TCR_9X('92', vNum_MTrans_tc92 ,
                         vBatch_Number_tc92,vNum_Tcrs_tc92,
                         vNum_Trns_tc92,vSource_Amount_tc92,vFechac6) ;

  pqOutput.PUT_LINE(linea);
   dbms_output.put_line('**** -----------------------------------------------------------------------');
   dbms_output.put_line('**** TOME NOTA DE ESTE NUMERO PARA EL CUADRE CONTRA EL CLEARING ');
   dbms_output.put_line('**** OUTGOING DE VISA : ' || lpad(to_char(vNum_Trns_tc92) || ' TRANSACCIONES',52, ' '));
   dbms_output.put_line('**** -----------------------------------------------------------------------');

  vNum_MTrans_tc92 := 0;
  vNum_Tcrs_tc92 :=0;
  vNum_Trns_tc92 := 0;
  vSource_Amount_tc92:=0;
  vBatch_Number_tc92:=0;

  -- Fin Para el TC92
  -- FIN DE CLR_COPAE8001************************************/
  pqOutput.Close_file;--SE CERRO EL ARCHIVO OUTVIEEEEYYYYMMDD.DAT
  utl_file.fCLOSE(file_id1);-- SE CERRO EL ARCHIVO OUTVIEEEEYYMMDD.TXT
dbms_output.put_line('****************************************************************************');
   dbms_output.put_line('**** TERMINA GENERACION DE OUTGOING VISA : ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
   dbms_output.put_line('****************************************************************************');

  PQMONPROC.inslog(nIdProceso,'M','Termina Generacion de Outgoing Visa: ' || vFileOutVi || ' A LAS ' || TO_CHAR(SYSDATE, 'HH:MM:SS'));
  vMensaje := pqmonproc.updmonproc (nIdProceso, 'F');
  --FIN DE ARCHIVO OUTVIEEEEYYYYMMDD.DAT ********
 EXCEPTION
     WHEN r_error Then
     dbms_output.put_line('vSqlErrM: ' || vSqlErrM || ' '|| ' pIdProceso:' || nIdProceso);
          PQMONPROC.InsLog(nIdProceso, 'E', vSqlErrM || ' - PROCESANDO CURSOR COPAE8001 - ' || vDesc_EntAdq);
          vMensaje:=PQMONPROC.UpdMonProc(nIdProceso, 'E','99');
          PQMONPROC.inslog(nIdProceso,'E','INFORME INMEDIATAMENTE AL ANALISTA DE TURNO DEL PROYECTO PMP VENEZUELA ','99');
          RETURN;
 END p_OutGoingVisa_rev;

---------------------------------------------------------------------
PROCEDURE p_CargaRegRechazados_rev (pCod_EntAdq varchar2,
                                pFechac6 varchar2,
                                pFile in out UTL_FILE.FILE_TYPE,
                                vNum_Tcrs_tc91 in out number,
                                vSource_Amount_tc91 in out number,
                                vNum_Tcrs_tc92 in out number,
                                vSource_Amount_tc92 in out number,
                                vNum_MTrans_tc91 in out number,
                                vNum_Trns_tc91 in out number,
                                vNum_MTrans_tc92 in out number,
                                vNum_Trns_tc92 in out number,
                                vBatch_Number_tc91 in out number,
                                vBatch_Number_tc92 in out number)
IS

    CURSOR Cr_VisaRechret IS
    SELECT id_rechret, cod_entadq, fila, fec_sesion, fec_upd, tipo_incoming, estado, observaciones, decode(trn_code,'10','20','20','10') trn_code,
           trn_code_qlf, trn_comp_seq_num, account_num, account_num_ext, floor_limit_ind, crb_excep_file_ind,
           pcas_ind, acq_ref_num, acq_bus_id, purchase_date, source_amount, source_curren_code, merch_name,
           merch_city, merch_count_code, merch_cate_code, merch_zip_code, merch_prov_code, req_payment_serv,
           usage_code, request_reason_code, settlement_flag, aut_charact_ind, authorization_code, pos_capability,
           inter_fee_ind, cardholder_id_met, collec_only_flag, pos_entry_mode, central_proc_date, reimb_attribute,
           iss_workstat_bin, acq_workstat_bin, chargeback_ref_num, documentation_ind, member_mess_text, special_cond_ind_1,
           special_cond_ind_2, fee_program_ind, card_acceptor_id, terminal_id, national_reimb_fee, mail_phone_ind,
           spec_chargeback_ind, interface_trace_num, cardhold_actter_ind, prepaid_card_ind, serv_develop_field,
           avs_response_code, aut_source_code, purchase_iden_form, atm_account_select, inst_payment_count,
           purchase_identifier, cashback, chip_condition_code, pos_enviroment
      FROM VisaRechret
     WHERE FEC_UPD = to_date(pFechac6,'YYMMDD')
       AND ESTADO = 'T'
       AND cod_entadq = pCod_EntAdq
       AND trn_code in ('10','20');

    reg_vr      Cr_VisaRechret%ROWTYPE;
    vTCR_10_20  TCR_10_20;
    Linea       varchar2(500);
    dfecha8     date;

BEGIN

   PQMONPROC.inslog(nIdProceso,'M','Cod_EntAdq:'||pCod_EntAdq);
   PQMONPROC.inslog(nIdProceso,'M','vFechac6:'||pFechac6);

    OPEN Cr_VisaRechret;
    LOOP
        BEGIN
            FETCH Cr_VisaRechret INTO reg_vr;
            EXIT WHEN Cr_VisaRechret%NOTFOUND;

            PQMONPROC.inslog(nIdProceso,'M','reg_vr.trn_code:'||reg_vr.trn_code);
               vTCR_10_20.Trn_Code := reg_vr.trn_code;
               vTCR_10_20.Trn_Code_Qlf:= '0';
               vTCR_10_20.Trn_Comp_Seq_Num:=  '0';
               vTCR_10_20.Destination_BIN := '000000';
               IF reg_vr.cod_entadq = 'BM' THEN
                  vTCR_10_20.Source_BIN := '411032';
               ELSIF reg_vr.cod_entadq = 'BP' THEN
                  vTCR_10_20.Source_BIN := '420198';
               END IF;
               vTCR_10_20.Reason_Code := reg_vr.REQUEST_REASON_CODE;
               vTCR_10_20.Country_Code := f_Country_Code(vTCR_10_20.Reason_Code);
                vTCR_10_20.Event_Date:= SUBSTR(pFechac6,3,4);
               vTCR_10_20.Account_Num := reg_vr.account_num;
               vTCR_10_20.Dest_Amount:= 0;
               vTCR_10_20.Dest_Curren_Code := '   ';
               vTCR_10_20.Source_Amount := reg_vr.source_amount;
               vTCR_10_20.Source_Curren_Code:= reg_vr.source_curren_code;
               vTCR_10_20.Mess_Text:= RPAD(reg_vr.MEMBER_MESS_TEXT,70,' ');
               vTCR_10_20.Settlement_Flag := reg_vr.SETTLEMENT_FLAG;
               vTCR_10_20.Trans_Identifier:= atleastone_TID(reg_vr.ACQ_REF_NUM);
               vTCR_10_20.Reserved_01:= ' ';
               vTCR_10_20.Central_Proc_Date:= '0000';
               vTCR_10_20.Reimb_Attribute:='0';

               linea := vTCR_10_20.Trn_Code ||  vTCR_10_20.Trn_Code_Qlf ||   vTCR_10_20.Trn_Comp_Seq_Num||
                        lpad(vTCR_10_20.Destination_BIN,6,'0') || lpad(vTCR_10_20.Source_BIN,6,'0') ||   lpad(vTCR_10_20.Reason_Code,4,'0') ||
                        vTCR_10_20.Country_Code || lpad(vTCR_10_20.Event_Date,4,'0') ||   vTCR_10_20.Account_Num ||
                        lpad(vTCR_10_20.Dest_Amount,12,'0') || vTCR_10_20.Dest_Curren_Code ||
                        lpad(vTCR_10_20.Source_Amount,12,'0') || vTCR_10_20.Source_Curren_Code || vTCR_10_20.Mess_Text ||
                        vTCR_10_20.Settlement_Flag || LPAD(vTCR_10_20.Trans_Identifier,15,'0') ||  vTCR_10_20.Reserved_01 ||
                        vTCR_10_20.Central_Proc_Date || vTCR_10_20.Reimb_Attribute;

               pqOutput.PUT_LINE(linea);

               utl_file.PUT_LINE( pFile, linea);

               p_Update_Contadores(vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, '1', vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.Source_Amount, 0);
               dfecha8 := TO_DATE(pFechac6,'YYMMDD');
               p_Inserta_10_20(TO_CHAR(dfecha8,'YYYYMMDD'), vTCR_10_20.Source_BIN, vTCR_10_20.Trn_Code, vTCR_10_20.Source_Curren_Code, vTCR_10_20.Settlement_Flag, vTCR_10_20.reason_code, vTCR_10_20.Source_Amount);

               --**********Para el TC91************
               vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_10_20.Source_Amount;
               vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_10_20.Source_Amount;
               vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
               vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
               vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
               vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's
               vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
               vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
               IF vNum_MTrans_tc91 = 500 THEN
                  vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's..91
                  vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;--Numero de TCR's..92
                  vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                  vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                  vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                  vBatch_Number_tc92 := vBatch_Number_tc92 + 1;

                  linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                           vBatch_Number_tc91,vNum_Tcrs_tc91,
                           vNum_Trns_tc91,vSource_Amount_tc91,pFechac6) ;

                  pqOutput.PUT_LINE(linea);

                  vNum_MTrans_tc91 := 0;
                  vNum_Tcrs_tc91 :=0;
                  vNum_Trns_tc91 := 0;
                  vSource_Amount_tc91:= 0;
               END IF;

        EXCEPTION
           WHEN Others THEN
                dbms_output.put_line('Error en cursor Visa Rechazados:' || SQLERRM);
                RETURN;
        END;
    END LOOP;

END;
--------------------------------------------------------------------------------
PROCEDURE p_CargaRegRechazados_ventas_r (pCod_EntAdq varchar2,
                                       pFechac6 varchar2,
                                       pFile in out UTL_FILE.FILE_TYPE,
                                       vNum_Tcrs_tc91 in out number,
                                       vSource_Amount_tc91 in out number,
                                       vNum_Tcrs_tc92 in out number,
                                       vSource_Amount_tc92 in out number,
                                       vNum_MTrans_tc91 in out number,
                                       vNum_Trns_tc91 in out number,
                                       vNum_MTrans_tc92 in out number,
                                       vNum_Trns_tc92 in out number,
                                       vBatch_Number_tc91 in out number,
                                       vBatch_Number_tc92 in out number)
IS

---tcr_5  y tcr_7
VTTCRIP_P55_ACTC CLR_DCEMVFULL.ttcrip_p55_actc%TYPE;
VNUMSEC_P23_ACTC CLR_DCEMVFULL.numsec_p23_actc%TYPE;
VFECCRI_P55_ACTC CLR_DCEMVFULL.feccri_p55_actc%TYPE;
VCAPTER_P55_ACTC CLR_DCEMVFULL.capter_p55_actc%TYPE;
VPAICRI_P55_ACTC CLR_DCEMVFULL.paicri_p55_actc%TYPE;
VNUMALE_P55_ACTC CLR_DCEMVFULL.numale_p55_actc%TYPE;
VEMVATC_P55_ACTC CLR_DCEMVFULL.emvatc_p55_actc%TYPE;
VPERINT_P55_ACTC CLR_DCEMVFULL.perint_p55_actc%TYPE;
VCRIPPE_P55_ACTC CLR_DCEMVFULL.crippe_p55_actc%TYPE;
VDAPLEM_P55_ACTC CLR_DCEMVFULL.daplem_p55_actc%TYPE;
VEMVTVR_P55_ACTC CLR_DCEMVFULL.emvtvr_p55_actc%TYPE;
VIMPCRI_P55_ACTC CLR_DCEMVFULL.impcri_p55_actc%TYPE;
VSCREMI_P55_ACTC CLR_DCEMVFULL.scremi_p55_actc%TYPE;
VRESCRI_P55_ACTC CLR_DCEMVFULL.rescri_p55_actc%TYPE;


    CURSOR Cr_VisaRechret IS
    SELECT id_rechret, cod_entadq, fila, fec_sesion, fec_upd, tipo_incoming, estado, observaciones, trn_code, --decode(trn_code,'05','25','25','05') trn_code,
           trn_code_qlf, trn_comp_seq_num, account_num, account_num_ext, floor_limit_ind, crb_excep_file_ind,
           pcas_ind, acq_ref_num, acq_bus_id, purchase_date, source_amount, source_curren_code, merch_name,
           merch_city, merch_count_code, merch_cate_code, merch_zip_code, merch_prov_code, req_payment_serv,
           usage_code, request_reason_code, settlement_flag, aut_charact_ind, authorization_code, pos_capability,
           inter_fee_ind, cardholder_id_met, collec_only_flag, pos_entry_mode, central_proc_date, reimb_attribute,
           iss_workstat_bin, acq_workstat_bin, chargeback_ref_num, documentation_ind, member_mess_text, special_cond_ind_1,
           special_cond_ind_2, fee_program_ind, card_acceptor_id, terminal_id, national_reimb_fee, mail_phone_ind,
           spec_chargeback_ind, interface_trace_num, cardhold_actter_ind, prepaid_card_ind, serv_develop_field,
           avs_response_code, aut_source_code, purchase_iden_form, atm_account_select, inst_payment_count,
           purchase_identifier, cashback, chip_condition_code, pos_enviroment
      FROM VisaRechret
     WHERE FEC_UPD = to_date(pFechac6,'YYMMDD')
       AND ESTADO = 'T'
       AND cod_entadq = pCod_EntAdq
       AND trn_code not in ('10','20');

    kSCREMI_P55_ACTC_DEF CHAR(10):= '          ';
    reg_vr       Cr_VisaRechret%ROWTYPE;
    vTCR_0          TCR_0;
    vTCR_1       TCR_1;
    vTCR_5       TCR_5;
    vTCR_7       TCR_7;
    Linea        VARCHAR2(500);
    VCUENTA      NUMBER:=0;

    p_IDEMEN_P00 NUMBER(4);
    p_CODPRO_P03 VARCHAR2(6);
    p_IMPTRA_P04 VARCHAR2(12);
    p_IDETRA_P11 VARCHAR2(6);
    p_TIMLOC_P12 VARCHAR2(12);
    p_PUNSER_P22 VARCHAR2(12);
    p_IDEADQ_P32 VARCHAR2(6);
    p_IDETER_P41 VARCHAR2(8);
    p_MONTRA_P49 VARCHAR2(3);
    P_TransactionIdentifier VARCHAR2(15);
    P_TID        VARCHAR2(15);


wkIdPart   CHAR(4);                    /* OBTENER LA PARTICION */
wkIdMes   NUMBER;               /*  OBTENER LA SUBPARTICION */

BEGIN

   PQMONPROC.inslog(nIdProceso,'M','Cod_EntAdq:'||pCod_EntAdq);
   PQMONPROC.inslog(nIdProceso,'M','vFechac6:'||pFechac6);

    OPEN Cr_VisaRechret;
    LOOP
        BEGIN
            FETCH Cr_VisaRechret INTO reg_vr;
            EXIT WHEN Cr_VisaRechret%NOTFOUND;

            PQMONPROC.inslog(nIdProceso,'M','reg_vr.trn_code:'||reg_vr.trn_code);

                vTCR_0.Trn_Code := reg_vr.trn_code;
                vTCR_0.Trn_Code_Qlf := '0';
                vTCR_0.Trn_Comp_Seq_Num := '0';
                vTCR_0.Account_Num := rpad(reg_vr.account_num,19,'0');
                vTCR_0.Floor_Limit_Ind:=' ';
                vTCR_0.CRB_Excep_File_Ind := ' ';
                vTCR_0.PCAS_ind := ' ';
                vTCR_0.Acq_Ref_Num := reg_vr.acq_ref_num;
                vTCR_0.Acq_Bus_ID := '00000000';
                vTCR_0.Purchase_date :=reg_vr.purchase_date;
                vTCR_0.Dest_Amount := '000000000000';
                vTCR_0.Dest_Curren_Code := '   ';
                vTCR_0.Source_Amount :=reg_vr.source_amount; -- numerico
                vTCR_0.Source_Curren_Code := reg_vr.source_curren_code;
                vTCR_0.Merch_Name := rpad(nvl(reg_vr.merch_name,' '),25,' ');
                vTCR_0.Merch_city := rpad(nvl(reg_vr.merch_city,' '), 13, ' ');
                vTCR_0.Merch_Count_Code := rpad(nvl(reg_vr.merch_count_code,' '),3, ' ');
                vTCR_0.Merch_Cate_Code  := rpad(nvl(reg_vr.merch_cate_code,' '),4, ' ');
                vTCR_0.Merch_Zip_Code   := '00000';
                vTCR_0.Merch_Prov_Code  := '   ';
                vTCR_0.Req_Payment_Serv :=' ';
                vTCR_0.Reserved_01 := ' ' ;
                vTCR_0.Usage_Code := rpad(nvl(reg_vr.usage_code,' '), 1, ' ');
                vTCR_0.Reason_Code  := nvl(reg_vr.request_reason_code,'00');
                vTCR_0.Settlement_Flag  := reg_vr.settlement_flag;
                vTCR_0.Aut_Charact_Ind := ' '; --reg_vr.aut_charact_ind;
                vTCR_0.Authorization_Code := reg_vr.authorization_code;
                vTCR_0.POS_Capability  := reg_vr.pos_capability;
                vTCR_0.Cardholder_ID_Met := reg_vr.cardholder_id_met;
                vTCR_0.Collec_Only_Flag :=' ';
                vTCR_0.POS_Entry_Mode := reg_vr.pos_entry_mode;
                vTCR_0.Inter_Fee_Ind := reg_vr.inter_fee_ind;
                IF vTCR_0.Trn_Code  in ('25','26') THEN
                    vTCR_0.Central_Proc_Date := reg_vr.central_proc_date;
                ELSE
                    vTCR_0.Central_Proc_Date := '0000'; --reg_vr.central_proc_date;
                END IF;
                vTCR_0.Reimb_Attribute := reg_vr.reimb_attribute;

                linea := lpad(vTCR_0.Trn_Code,2,'0')                || vTCR_0.Trn_Code_Qlf             || vTCR_0.Trn_Comp_Seq_Num        ||
                         vTCR_0.Account_Num                         || vTCR_0.Floor_Limit_Ind          || vTCR_0.CRB_Excep_File_Ind      ||
                         vTCR_0.PCAS_ind                            || vTCR_0.Acq_Ref_Num              || lpad(vTCR_0.Acq_Bus_ID,8,'0')  ||
                         vTCR_0.Purchase_date                       || lpad(vTCR_0.Dest_Amount,12,'0') || vTCR_0.Dest_Curren_Code        ||
                         lpad(to_char(vTCR_0.Source_Amount),12,'0') || vTCR_0.Source_Curren_Code       || vTCR_0.Merch_Name              ||
                         vTCR_0.Merch_city                          || vTCR_0.Merch_Count_Code         || vTCR_0.Merch_Cate_Code         ||
                         vTCR_0.Merch_Zip_Code                      || vTCR_0.Merch_Prov_Code          || vTCR_0.Req_Payment_Serv        ||
                         vTCR_0.Reserved_01                         || vTCR_0.Usage_Code               || lpad(vTCR_0.Reason_Code,2,'0') ||
                         vTCR_0.Settlement_Flag                     || vTCR_0.Aut_Charact_Ind          || vTCR_0.Authorization_Code      ||
                         vTCR_0.POS_Capability                      || vTCR_0.Inter_Fee_Ind            || vTCR_0.Cardholder_ID_Met       ||
                         vTCR_0.Collec_Only_Flag                    || vTCR_0.POS_Entry_Mode           || vTCR_0.Central_Proc_Date       ||
                         vTCR_0.Reimb_Attribute;

                pqOutput.PUT_LINE(linea);
                --  Generacion de OUTVIYYYYMMDD.TXT
                IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                    utl_file.PUT_LINE( pFile, linea);
                ELSE
                    IF vTCR_0.Trn_Code IN ('06','25','26') THEN
                        utl_file.PUT_LINE( pFile, linea);
                    END IF;
                END IF;
                --  Fin de Generacion de OUTVIYYYYMMDD.TXT

                -- **** p_Update_Contadores(SUBSTR(vTCR_0.Acq_Ref_Num,2,6), vtcr_0.Trn_Code, vtcr_0.Usage_Code, vtcr_0.Source_Curren_Code, vtcr_0.Settlement_Flag, vtcr_0.Source_Amount, rCr_EX8001.IMPCU1_P46_ACTC);

                -- Contadores para TC1
                vNum_Tcrs_tc91:= vNum_Tcrs_tc91+1;
                vSource_Amount_tc91 := vSource_Amount_tc91 + vTCR_0.Source_Amount;
                vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;
                vSource_Amount_tc92 := vSource_Amount_tc92 + vTCR_0.Source_Amount;

                -- Aqui falta algo mas Revisar

                vTCR_1.Trn_Code             := lpad(reg_vr.trn_code,2,'0');
                vTCR_1.Trn_Code_Qlf         :='0';
                vTCR_1.Trn_Comp_Seq_Num     :='1';
                vTCR_1.Iss_Workstat_BIN     := '      ';
                vTCR_1.Acq_Workstat_BIN     := '      ';
                vTCR_1.Chargeback_Ref_Num   := lpad(reg_vr.chargeback_ref_num,6,'0');
                vTCR_1.Documentation_Ind    := reg_vr.documentation_ind;
                vTCR_1.Member_Mess_Text     := rpad(reg_vr.member_mess_text,50,' ');
                vTCR_1.Special_Cond_Ind     := '  ';
                vTCR_1.FeeProgramID         := '   ';
                vTCR_1.Issuer_charge        := ' ';
                vTCR_1.Reserved_01          := ' ';
                vTCR_1.Card_Acceptor_ID     := lpad(reg_vr.card_acceptor_id, 15,'0');
              --vTCR_1.Terminal_ID := rCr_EX8001.IDETER_P41_ACTC;--Cambio pedido de AV 06 SEP 2004
                vTCR_1.Terminal_ID          := rpad(nvl(reg_vr.terminal_id,' '),8,' ');
                vTCR_1.National_Reimb_Fee   := f_Nat_Reimbursement_fee_06(vTCR_0.Settlement_Flag,reg_vr.national_reimb_fee); --reg_vr.national_reimb_fee;reg_vr.national_reimb_fee;
                vTCR_1.Mail_Phone_Ind       := reg_vr.mail_phone_ind;
                vTCR_1.Spec_Chargeback_Ind := ' ';
                vTCR_1.Interface_Trace_Num := '000000';
            --  vTCR_1.Cardhold_ActTer_Ind := ' '; --comentado pedido AV
                vTCR_1.Cardhold_ActTer_Ind := reg_vr.cardhold_actter_ind;
                vTCR_1.Prepaid_Card_Ind := ' ';
                vTCR_1.Reserved_02 := ' ';
                vTCR_1.AVS_Response_Code := ' ';
                vTCR_1.Aut_Source_Code := ' ';
                vTCR_1.Purchase_Iden_Form := ' ';
                vTCR_1.ATM_Account_Select := '0';
                vTCR_1.Inst_Payment_Count := '  ';
                vTCR_1.Purchase_Identifier := '                         ';
             -- vTCR_1.Cashback := '000000000';
                vTCR_1.Cashback := lpad(nvl(reg_vr.cashback,0),9,'0');
                vTCR_1.Chip_Cond_Code := reg_vr.chip_condition_code;
                vTCR_1.POS_Environment  := reg_vr.pos_enviroment;

                Linea := vTCR_1.Trn_Code                        ||   vTCR_1.Trn_Code_Qlf                    ||  vTCR_1.Trn_Comp_Seq_Num                 ||
                         vTCR_1.Iss_Workstat_BIN                ||   vTCR_1.Acq_Workstat_BIN                ||  vTCR_1.Chargeback_Ref_Num               ||
                         vTCR_1.Documentation_Ind               ||   vTCR_1.Member_Mess_Text                ||  vTCR_1.Special_Cond_Ind                 ||
                         vTCR_1.FeeProgramID     ||  vTCR_1.Issuer_charge ||
                         vTCR_1.Reserved_01                     ||   vTCR_1.Card_Acceptor_ID                ||  vTCR_1.Terminal_ID                      ||
                         lpad(vTCR_1.National_Reimb_Fee,12,'0') ||   vTCR_1.Mail_Phone_Ind                  ||  vTCR_1.Spec_Chargeback_Ind              ||
                         vTCR_1.Interface_Trace_Num             ||   vTCR_1.Cardhold_ActTer_Ind             ||  vTCR_1.Prepaid_Card_Ind                 ||
                         vTCR_1.Reserved_02                     ||   vTCR_1.AVS_Response_Code               ||  vTCR_1.Aut_Source_Code                  ||
                         vTCR_1.Purchase_Iden_Form              ||   vTCR_1.ATM_Account_Select              ||  vTCR_1.Inst_Payment_Count               ||
                       --vTCR_1.Purchase_Identifier || vTCR_1.Cashback ||  vTCR_1.Chip_Cond_Code ||  vTCR_1.POS_Environment;
                         vTCR_1.Purchase_Identifier              ||   vTCR_1.Cashback                        ||  vTCR_1.Chip_Cond_Code                   ||
                         vTCR_1.POS_Environment;
                pqOutput.PUT_LINE(linea);
                --  Generacion de outviEEEEyyyymmdd.txt
                IF vTCR_0.Usage_Code='2' AND vTCR_0.Trn_Code ='05' THEN
                    utl_file.PUT_LINE( pFile, linea);
                ELSE
                   IF vTCR_0.Trn_Code IN ('06','25','26') THEN
                      utl_file.PUT_LINE( pFile , linea);
                   END IF;
                END IF;
            -- Contadores para tc91 y 92 del Tc07
               vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
               vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's
-----------------
--vTCR_5 y vTCR_7
-----------------

/*   IF reg_vr.chip_condition_code='1' THEN

      SELECT IDEMEN_P00_ACTC,CODPRO_P03_ACTC,IMPTRA_P04_ACTC, IDETRA_P11_ACTC,TIMLOC_P12_ACTC,IDEADQ_P32_ACTC,IDETER_P41_ACTC, MONTRA_P49_ACTC,FILLER_P62_ACTC
        INTO p_IDEMEN_P00   ,p_CODPRO_P03   ,p_IMPTRA_P04   ,p_IDETRA_P11   ,p_TIMLOC_P12   ,p_IDEADQ_P32   ,p_IDETER_P41    ,p_MONTRA_P49  , P_TransactionIdentifier
        FROM CLR_EX8001
       WHERE REFADQ_P31_ACTC =trim(reg_vr.acq_ref_num);
*/
      IF reg_vr.cod_entadq = 'BM' THEN

        SELECT p00idmsg   ,lpad(p03codpro,6,'0'),p04imptra    ,LPAD(LTRIM(p11idetra),6,'0')    ,TO_CHAR(TO_DATE(RTRIM(P12TIMLOC),'YYYYMMDDHH24MISS'),'YYMMDDHH24MISS'), p22punser, p32idadq     ,p41cseri     ,p49montra
                    ,idparticion||SUBSTR(p28sesion,5,2)||TO_CHAR(TO_DATE(p28sesion,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion,id_mes
        INTO p_IDEMEN_P00 ,p_CODPRO_P03         ,p_IMPTRA_P04 ,p_IDETRA_P11 ,p_TIMLOC_P12 ,p_PUNSER_P22, p_IDEADQ_P32 ,p_IDETER_P41 ,p_MONTRA_P49,wkIdPart,wkIdMes
        FROM MCP_BM
        WHERE  p00idmsg = decode(lpad(reg_vr.trn_code,2,'0'),'25','1244','26','1244','05','1444','06','1444')
         AND p31refadq = trim(reg_vr.acq_ref_num);


      ELSIF reg_vr.cod_entadq = 'BP' THEN

        SELECT p00idmsg   ,lpad(p03codpro,6,'0'),p04imptra    ,LPAD(LTRIM(p11idetra),6,'0')    ,TO_CHAR(TO_DATE(RTRIM(P12TIMLOC),'YYYYMMDDHH24MISS'),'YYMMDDHH24MISS'), p22punser, p32idadq     ,p41cseri     ,p49montra
                    ,idparticion||SUBSTR(p28sesion,5,2)||TO_CHAR(TO_DATE(p28sesion,'YYMMDD'),'DAY','NLS_DATE_LANGUAGE=''numeric date language''') as idparticion,id_mes
        INTO p_IDEMEN_P00 ,p_CODPRO_P03         ,p_IMPTRA_P04 ,p_IDETRA_P11 ,p_TIMLOC_P12 ,p_PUNSER_P22, p_IDEADQ_P32 ,p_IDETER_P41 ,p_MONTRA_P49,wkIdPart,wkIdMes
         FROM MCP_BP
        WHERE  p00idmsg = decode(lpad(reg_vr.trn_code,2,'0'),'25','1244','26','1244','05','1444','06','1444')
         AND p31refadq = trim(reg_vr.acq_ref_num);

      END IF;

      IF SUBSTR(p_PUNSER_P22,7,1) = '5' THEN

          SELECT COUNT(1)
             INTO VCUENTA
             FROM CLR_DCEMVFULL
            WHERE IDEMEN_P00_ACTC = p_IDEMEN_P00 --DECODE(p_IDEMEN_P00,'1240','1244','1440','1444')
              AND IDETRA_P11_ACTC = p_IDETRA_P11
              AND TIMLOC_P12_ACTC = p_TIMLOC_P12
              AND IDEADQ_P32_ACTC = p_IDEADQ_P32
              AND  idparticion         =wkIdPart   /* Implementaci¿n del particionamiento TST 19/04/2014 */
             AND  id_mes              =wkIdMes;  /* Implementaci¿n del particionamiento TST 19/04/2014 */


          IF VCUENTA > 0  THEN

               SELECT TTCRIP_P55_ACTC,NUMSEC_P23_ACTC,FECCRI_P55_ACTC,CAPTER_P55_ACTC,
                      PAICRI_P55_ACTC,NUMALE_P55_ACTC,EMVATC_P55_ACTC,PERINT_P55_ACTC,
                      CRIPPE_P55_ACTC,DAPLEM_P55_ACTC,EMVTVR_P55_ACTC,IMPCRI_P55_ACTC,
                      SCREMI_P55_ACTC,RESCRI_P55_ACTC
                 INTO VTTCRIP_P55_ACTC,VNUMSEC_P23_ACTC,VFECCRI_P55_ACTC,VCAPTER_P55_ACTC,
                      VPAICRI_P55_ACTC,VNUMALE_P55_ACTC,VEMVATC_P55_ACTC,VPERINT_P55_ACTC,
                      VCRIPPE_P55_ACTC,VDAPLEM_P55_ACTC,VEMVTVR_P55_ACTC,VIMPCRI_P55_ACTC,
                      VSCREMI_P55_ACTC,VRESCRI_P55_ACTC
                 FROM CLR_DCEMVFULL
                WHERE IDEMEN_P00_ACTC = p_IDEMEN_P00 --DECODE(p_IDEMEN_P00,'1240','1244','1440','1444')
                  AND IDETRA_P11_ACTC = p_IDETRA_P11
                  AND TIMLOC_P12_ACTC = p_TIMLOC_P12
                  AND IDEADQ_P32_ACTC = p_IDEADQ_P32
                  AND  idparticion         =wkIdPart   /* Implementaci¿n del particionamiento TST 19/04/2014 */
                 AND  id_mes              =wkIdMes;  /* Implementaci¿n del particionamiento TST 19/04/2014 */


                  P_TID := SGCVNZ.PQOUTGOINGVISA.atleastone_TID(trim(reg_vr.acq_ref_num));

             vTCR_5.Trn_Code                       := lpad(reg_vr.trn_code,2,'0'); --f_Trn_Code(p_IDEMEN_P00,p_CODPRO_P03,NULL);
             vTCR_5.Trn_Code_Qlf                   := '0';
             vTCR_5.Trn_Comp_Seq_Num               := '5';
             vTCR_5.TransactionIdentifier          := P_TID; -- P_TransactionIdentifier ; --NVL(substr( P_TransactionIdentifier,1,15), LPAD('0',15,'0')); --carta tecnica visa 2013 inclusin del TID MODIF POR FELIPE VITALE
           -- dbms_output.put_line('ENTRO 6:' || vTCR_5.TransactionIdentifier);

           vTCR_5.AuthorizedAmount               := LPAD(TO_CHAR(p_IMPTRA_P04),12,'0');
             vTCR_5.AuthorizationCurrencyCode      := LPAD(p_MONTRA_P49,3,' ');
             vTCR_5.AuthorizationResponseCode      := LPAD(NVL(TRIM(VRESCRI_P55_ACTC),0),2,'0'); --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0');  --LPAD(TRIM(VRESCRI_P55_ACTC),2,'0'); --LPAD(VRESCRI_P55_ACTC,2,'0');
             vTCR_5.ValidationCode                 := LPAD(' ',4,' ');
             vTCR_5.ExcludedTranIdenReason         := LPAD(' ',1,' ');
             vTCR_5.CRSProcessingCode              := LPAD(' ',1,' ');
             vTCR_5.ChargebackRightsIndicator      := LPAD(' ',2,' ');
             vTCR_5.MultipleClearingSequenceNumber := LPAD('0',2,'0');
             vTCR_5.MultipleClearingSequenceCount  := LPAD('0',2,'0');
             vTCR_5.MarketSpecificAutDataInd       := LPAD(' ',1,' ');
             vTCR_5.TotalAuthorizedAmount          := LPAD('0',12,'0');
             vTCR_5.InformationIndicator           := LPAD(' ',1,' ');
             vTCR_5.MerchantTelephoneNumber        := LPAD(' ',14,' ');
             vTCR_5.AdditionalDataIndicator        := LPAD(' ',1,' ');
             vTCR_5.MerchantVolumeIndicator        := LPAD(' ',2,' ');
             vTCR_5.ElectronicCommGoodsInd         := LPAD(' ',2,' ');
             vTCR_5.MerchantVerificationValue      := LPAD(' ',10,' ');
             vTCR_5.InterchangeFeeAmount           := LPAD('0',15,'0');
             vTCR_5.InterchangeFeeSign             := LPAD(' ',1,' ');
             vTCR_5.SourceCurrBaseCurrExchRate     := LPAD('0',8,'0');
             vTCR_5.BaseCurrtoDestCurrExchRate     := LPAD('0',8,'0');
             vTCR_5.OptionalIssuerISAAmount        := LPAD('0',12,'0');
             vTCR_5.ProductID                      := LPAD(' ',2,' ');
             vTCR_5.ProgramID                      := LPAD(' ',6,' ');
             vTCR_5.Reserved                       := LPAD(' ',24,' ');
             vTCR_5.CVV2ResultCode                 := LPAD(' ',1,' ');

             IF TRIM(vTCR_5.ProductID) = 'I' THEN
                 vTCR_5.ProductID := 'I1';
             END IF;


             linea := LPAD(vTCR_5.Trn_Code,2,'0')||vTCR_5.Trn_Code_Qlf||
                      vTCR_5.Trn_Comp_Seq_Num||vTCR_5.TransactionIdentifier||
                      vTCR_5.AuthorizedAmount||vTCR_5.AuthorizationCurrencyCode||
                      vTCR_5.AuthorizationResponseCode||vTCR_5.ValidationCode||
                      vTCR_5.ExcludedTranIdenReason||vTCR_5.CRSProcessingCode||
                      vTCR_5.ChargebackRightsIndicator||vTCR_5.MultipleClearingSequenceNumber||
                      vTCR_5.MultipleClearingSequenceCount||vTCR_5.MarketSpecificAutDataInd||
                      vTCR_5.TotalAuthorizedAmount||vTCR_5.InformationIndicator||
                      vTCR_5.MerchantTelephoneNumber||vTCR_5.AdditionalDataIndicator||
                      vTCR_5.MerchantVolumeIndicator||vTCR_5.ElectronicCommGoodsInd||
                      vTCR_5.MerchantVerificationValue||vTCR_5.InterchangeFeeAmount||
                      vTCR_5.InterchangeFeeSign||vTCR_5.SourceCurrBaseCurrExchRate||
                      vTCR_5.BaseCurrtoDestCurrExchRate||vTCR_5.OptionalIssuerISAAmount||
                      vTCR_5.ProductID||vTCR_5.ProgramID||
                      vTCR_5.Reserved||
                      vTCR_5.CVV2ResultCode;


             pqOutput.PUT_LINE(linea);

             dbms_output.put_line('ENTRO 6:' || linea);

             vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;
             vNum_Tcrs_tc92:=vNum_Tcrs_tc92+1;


             vTCR_7.Trn_Code := lpad(reg_vr.trn_code,2,'0'); --f_Trn_Code(p_IDEMEN_P00,p_CODPRO_P03,NULL);
             vTCR_7.Trn_Code_Qlf := '0';
             vTCR_7.Trn_Comp_Seq_Num := '7';
             vTCR_7.dctcttcrip := LPAD(NVL(VTTCRIP_P55_ACTC,'0'),2,'0');
             vTCR_7.dcnumsec := LPAD(NVL(VNUMSEC_P23_ACTC,0),3,'0');
             vTCR_7.dctcfeccri := LPAD(VFECCRI_P55_ACTC,6,'0');
             vTCR_7.dctccapter := LPAD(VCAPTER_P55_ACTC,6,'0');
             vTCR_7.dctcpaicri :=  '862'; --RPAD(VPAICRI_P55_ACTC,3,' ');
             vTCR_7.Num_serie :=  LPAD(' ',8,' '); --LPAD(p_IDETER_P41,8,' ');
             vTCR_7.dctcnumale := LPAD(VNUMALE_P55_ACTC,8,'0');
             vTCR_7.dctcemvatc := LPAD(VEMVATC_P55_ACTC,4,'0');
             vTCR_7.dctcperint := LPAD(VPERINT_P55_ACTC,4,'0');
             vTCR_7.dctccrippe := LPAD(VCRIPPE_P55_ACTC,16,'0');
             vTCR_7.dctcdaplem01 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),3,2),2,'0');
             vTCR_7.dctcdaplem02 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),5,2),2,'0');
             vTCR_7.dctcemvtvr := LPAD(VEMVTVR_P55_ACTC,10,'0');
             vTCR_7.dctcdaplem03 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),7,8),8,'0');
             vTCR_7.dctcimpcri := LPAD(VIMPCRI_P55_ACTC,12,'0');
             vTCR_7.dctcdaplem04 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),15,2),2,'0');
             vTCR_7.dctcdaplem05 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),17,16),16,'0');
             vTCR_7.dctcdaplem06 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),1,2),2,'0');
             vTCR_7.dctcdaplem07 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),33,2),2,'0');
             vTCR_7.dctcdaplem08 := LPAD(SUBSTR(STD.F_RTRIM4HEX(VDAPLEM_P55_ACTC),35,30),30,'0');
             vTCR_7.Espacios := LPAD(' ',8,' ');
             vTCR_7.dctcscremi := NVL(LPAD(STD.F_RTRIM4HEX(VSCREMI_P55_ACTC),10,'0'),kSCREMI_P55_ACTC_DEF);

             linea := LPAD(vTCR_7.Trn_Code,2,'0') || vTCR_7.Trn_Code_Qlf || vTCR_7.Trn_Comp_Seq_Num ||
                      vTCR_7.dctcttcrip || LPAD(vTCR_7.dcnumsec,3,'0')|| vTCR_7.dctcfeccri||vTCR_7.dctccapter||
                      vTCR_7.dctcpaicri||vTCR_7.Num_serie||vTCR_7.dctcnumale||vTCR_7.dctcemvatc||
                      vTCR_7.dctcperint||vTCR_7.dctccrippe||vTCR_7.dctcdaplem01||vTCR_7.dctcdaplem02||
                      vTCR_7.dctcemvtvr||vTCR_7.dctcdaplem03||vTCR_7.dctcimpcri||vTCR_7.dctcdaplem04||
                      vTCR_7.dctcdaplem05||vTCR_7.dctcdaplem06||vTCR_7.dctcdaplem07||vTCR_7.dctcdaplem08||
                      vTCR_7.Espacios||vTCR_7.dctcscremi;

            pqOutput.PUT_LINE(linea);

           vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
           vNum_Tcrs_tc92:= vNum_Tcrs_tc92+1;--Numero de TCR's

          END IF;

   END IF;


            --  Fin de Generacion de OutviEEEEyyyymmdd.txt
            -- ********** Para el TC91 ************
--                vNum_Tcrs_tc91:=vNum_Tcrs_tc91+1;--Numero de TCR's
                vNum_MTrans_tc91 := vNum_MTrans_tc91 + 1;--transacciones
                vNum_Trns_tc91 := vNum_Trns_tc91 + 1; --transacciones contables
--                vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's
                vNum_MTrans_tc92 := vNum_MTrans_tc92 + 1;
                vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                IF vNum_MTrans_tc91 = 500 THEN
                   vNum_Tcrs_tc91 := vNum_Tcrs_tc91+1;--Numero de TCR's..91
                   vNum_Tcrs_tc92 := vNum_Tcrs_tc92+1;--Numero de TCR's..92
                   vNum_Trns_tc91 := vNum_Trns_tc91 + 1;
                   vNum_Trns_tc92 := vNum_Trns_tc92 + 1;
                   vBatch_Number_tc91 := vBatch_Number_tc91 + 1;
                   vBatch_Number_tc92 := vBatch_Number_tc92 + 1;
                   linea := f_gen_TCR_9X('91', vNum_MTrans_tc91 ,
                            vBatch_Number_tc91,vNum_Tcrs_tc91,
                            vNum_Trns_tc91,vSource_Amount_tc91,pFechac6) ;
                    pqOutput.PUT_LINE(linea);
                    vNum_MTrans_tc91 := 0;
                    vNum_Tcrs_tc91 := 0;
                    vNum_Trns_tc91 := 0;
                    vSource_Amount_tc91 := 0;
                END IF;

        EXCEPTION
           WHEN Others THEN
                dbms_output.put_line('Error en cursor Visa Rechazados:' || SQLERRM);
                RETURN;
        END;
    END LOOP;

END;

 /* modificaciones de Mandatory VISA 2041 Felipe Vitale 9-4-2014
 2.1 Changes to MerchantCategory Codes
 2.10 New Visa Infinite Business Product and Interchange Fee Programs
 2.11 BASE II Draft Data, TCR 5¿Payment Service Data
Visa will continue to include the appropriate value in the Spend Qualified Indicator field in
BASE II Draft Data, TCR 5, position 149, prior to delivery to the destination. There are no
changes to the existing BASE II Draft Data record layout.
The existing spend qualified indicator value N and new value Q will continue to be
determined by Visa based on account-level information that resides in Visa systems.*/

END PQOUTGOINGVISA;
/
GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SGCBM;
GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SGCBP;
GRANT EXECUTE ON SGCVNZ.PQOUTGOINGVISA TO ROLE_SOPTECN;
