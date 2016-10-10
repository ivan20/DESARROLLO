create or replace
PACKAGE BODY "YPKG_PROMOCIONES"
/* $Header:: /PRODUCCION/PAQUETES/BODY/YPKG_PROMOCIONES_b.sql 9 1.1.3 12.09.16 14:41 PSADM                                                                                                                                                         $ */
IS
-- MODIFICATION HISTORY
-- Person Date Comments
-- --------------- ---------- --------------------------------------------------------------------------------------------------
-- Hector Miranda 14.01.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Creacion del paquete con primeras funciones.
-- Hector Miranda 24.03.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Creacion de la funciOn fv_Get_Promo y fv_devuelveListaCat.
-- Hector Miranda 24.03.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Modificacion en el query del cursor de la funcion fn_queryPromo.
-- Hector Miranda 01.04.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Creacion de la funcion fn_retornaprecio.
-- Hector Miranda 08.04.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Actualizar la funcion fv_Get_Promo.
-- Hector Miranda 14.04.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Creacion de la funcion fn_Field_Accounts
-- Hector Miranda 21.04.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Actualizacion de la funcion fv_Get_Promo
-- Margarita Castillo 21.04.2014 GPP-IT-606-OPTIMIZACION-REGLAS-BS. Actualizacion de la funcion Fn_RetornaPrecio
-- Dariusz Gadowski (CA) 21.04.2014 [CR][W5103K090] - Analysis for migration to 6.6 - code optimisation: Using oracle Cache (PL/SQL tables and new cached function fvGetLookupValue)
-- to optimise following functions: fv_Get_Promo (cache result of promotion catalogue), fv_devuelveListaCat (cache result of list values),fn_queryPromo (Add ROWNUM <=1 in count(1))
-- Hector Miranda 04.02.2015 Actualizacion de los procedimeintos y funciones para el control de Nodos(fn_queryPromoNew,fn_queryPromo,fv_Get_Promo)
-- Hector Miranda 04.03.2015 Actualizacion de los procedimientos y funciones (fn_queryPromoNew,fn_queryPromo,fv_Get_Promo) para instalacion
-- Carlos Ca?ar 01.04.2015 GPP-DYM-PRY_849-NUEVO_MODULO_PROMOCIONES_INSERT ACTUALIZACION DE LAS FUNCIONES Y PROCEDIMIENTOS (fn_queryPromoNew,fn_queryPromo,fv_Get_Promo)
-- Carlos Ca?ar 08.04.2015 GPP-DYM-PRY_849-NUEVO_MODULO_PROMOCIONES_HD ACTUALIZACION DE LAS FUNCIONES Y PROCEDIMIENTOS PARA HD (fv_Get_Promo,fn_retornaprecio,rLoadPromotions)
-- Carlos Ca?ar 09.04.2015 GPP-DYM-PRY_849-NUEVO_MODULO_PROMOCIONES_HD ACTUALIZACION DE LAS FUNCIONES Y PROCEDIMIENTOS PARA HD (fn_retornaprecio)
-- Carlos Ca?ar 10.04.2015 GPP-DYM-PRY_849-NUEVO_MODULO_PROMOCIONES_HD ACTUALIZACION DE LAS FUNCIONES Y PROCEDIMIENTOS PARA HD (fn_retornaprecio)
-- Carlos Ca?ar 18.05.2015 Modificacion fv_Get_Promo linea 813 se cambio el valor 0 por el valor 503950
-- Carlos Ca?ar 25.05.2015 Modificacion linea 814 el cambio fue (cur.controlname = 'cblTipoServicio' AND vi_folder_id = 2 and (vv_tipo='M' or (vv_tipo='I' and nvl(vv_param(24),0) in (503950))) then)
-- Carlos Ca?ar 06.06.2015 Modificacion linea 1039 hasta 1045 Adicionales Tulcan
-- Carlos Ca?ar 06.06.2015 Modificacion linea 1082 hasta 1088 Adicionales Tulcan
-- Carlos Ca?ar 06.06.2015 Creacion de la Funcion fn_retornaprecio1 paara el PRY_882 y Modificacion (fn_queryPromoNew,fn_queryPromo,fv_Get_Promo)
--- para enviar el ServiceBegiDate y Creacion de un Procedimiento para Monitoreo de Promociones GUARDAR_LOGS
-- Carlos Ca?ar 08.06.2015 Creacion de la Funciones y Procedimiento PRY_849 Producto Nuevo Basico HD
-- Carlos Ca?ar 16.06.2015 Modificacion de la Funciones fn_retornaprecio PRY_882
-- Carlos Ca?ar 05.07.2015 Modificacion de la Funciones fn_retornaprecio1 modificacion en la obtencion del Precio
-- Carlos Ca?ar 18.08.2015  Modificacion de la Funciones fn_retornaprecio1, obtencion de id de promocion al cual entro el cliente
-- Carlos Ca?ar 18.08.2015  Modificacion de la Funciones fn_retornaprecio1 cuando no encuentre valor envie 1 millon
-- Carlos Ca?ar 16.10.2015  Modificacion de la Funciones fn_queryPromoNew y fv_devuelveListaCat cambio de tama?o de variables
-- Carlos Ca?ar 04.12.2015  Modificacion de la Funciones fn_retornaprecio1 para PAY CHANNEL(Adicional_Salinas)
-- Daniel Solano         10.12.2015  Creacion de la Funcion YFN_CITEMCESDESR para ver si viene de un CESDER
-- Daniel Solano         17.02.2016  Creación de la Funcion FN_RETORNAPRECIO1EMP para Empleados TVC para sacar la mensualidad sin descuentos
-----------------------------------------------------------------------------------------------------------------------------------
 -- cache table used in: fv_devuelveListaCat
 gt_DevuelveListaCats cTypes.VTableV;
 -----------------------------------------------------------------------------------------------------------------------------------
 -- cache record and PL/SQL type tables definition used in: fv_Get_Promo
 -----------------------------------------------------------------------------------------------------------------------------------
 TYPE TRec_PromotionCatalogue IS RECORD
 (catalogpromotions_id y_catalogpromotionsitems.catalogpromotions_id%TYPE
 ,controlname y_catalogpromotionsitems.controlname%TYPE
 ,NUMBERvalue VARCHAR2(4000)
 ,dfrom y_catalogpromotionsitems.dfrom%TYPE
 ,proyecto_id y_catalogpromotionsitems.proyecto_id%TYPE
 );
 TYPE TTab_PromotionCatalogue IS TABLE OF TRec_PromotionCatalogue INDEX BY BINARY_INTEGER;
 TYPE TTab_PromotionCatalogues IS TABLE OF TTab_PromotionCatalogue INDEX BY BINARY_INTEGER;
 -----------------------------------------------------------------------------------------------------------------------------------
 gt_PromotionCatalogues TTab_PromotionCatalogues;
 gt_ISPromotionCataloguesLoaded cTypes.VTableV;
 TYPE TRec_AccountFields IS RECORD
 (ProcInstanceId NUMBER
 ,Account_Id NUMBER
 ,CCenter_Id NUMBER
 ,Accounttype_id NUMBER
 ,CpartyType_id NUMBER
 ,PaymentType VARCHAR2(10)
 );
 gr_AccountFields TRec_AccountFields;
 TYPE TTab_VTable IS TABLE OF VARCHAR2(1000) INDEX BY VARCHAR2(1000);
 TYPE TTab_2VTable IS TABLE OF TTab_VTable INDEX BY VARCHAR2(1000);
 TYPE TTab_3VTable IS TABLE OF TTab_2VTable INDEX BY VARCHAR2(1000);
 SUBTYPE TRec_CatalogPromotion IS y_catalogpromotions%ROWTYPE;
 TYPE TTab_CatalogPromotions IS TABLE OF TRec_CatalogPromotion INDEX BY BINARY_INTEGER;
 SUBTYPE TRec_CatalogPromotionItem IS y_catalogpromotionsitems%ROWTYPE;
 TYPE TTab_CatalogPromotionItems IS TABLE OF TRec_CatalogPromotionItem INDEX BY BINARY_INTEGER;
 gt_CatalogPromotions TTab_CatalogPromotions;
 gt_CatalogPromotionItems TTab_CatalogPromotionItems;
 gt_CatProm2Items TTab_2VTable;
 gt_CatProm2ItemsCtrlValues TTab_2VTable;
 gt_CatProm2ItemsCtrl TTab_3VTable;
 /*
 Procedure cache results Promoton catalogue into PL/SQL table cache (gt_PromotionCatalogues)
 ----------------------------------------------------------------------------------------------------
 */
 PROCEDURE rLoadPromotionCatalogue
 IS
  vr_PromotioCat   TRec_PromotionCatalogue;
 BEGIN
 --#DG Load promo catalogue if not loaded
 IF gt_DevuelveListaCats.COUNT = 0 THEN
   rReLoadDevuelveListaCat;
 END IF;
 --
 IF gt_PromotionCatalogues.COUNT = 0 THEN
   FOR vr IN (SELECT *
              FROM ( SELECT UNIQUE t1.catalogpromotions_id
                                  ,t1.controlname
                                  ,TO_CHAR(NULL)  NUMBERvalue
                                  ,t1.dfrom
                                  ,t1.proyecto_id
                              FROM y_catalogpromotionsitems t1
                             WHERE isvalid = 'Y'
                          ORDER by catalogpromotions_id,t1.controlname
                   )
              )
   LOOP
      --
      vr_PromotioCat             := vr;
      vr_PromotioCat.NUMBERvalue := YPKG_PROMOCIONES.fv_DevuelveListaCatCached(vr.catalogpromotions_id, vr.controlname);
      --
      IF NOT gt_PromotionCatalogues.EXISTS(vr.catalogpromotions_id) THEN
         gt_PromotionCatalogues(vr.catalogpromotions_id)(1) := vr_PromotioCat;
         --
         gt_ISPromotionCataloguesLoaded(vr.catalogpromotions_id) := NULL;
         --
      ELSE
         gt_PromotionCatalogues(vr.catalogpromotions_id)(gt_PromotionCatalogues(vr.catalogpromotions_id).COUNT+1) := vr_PromotioCat;
      END IF;
      ---
   END LOOP;
 END IF;
 END rLoadPromotionCatalogue;
 /*
 Procedure cache results Promotons and items into PL/SQL
 tables cache (gt_CatalogPromotions,gt_CatalogPromotionItems)
 ----------------------------------------------------------------------------------------------------
 */
 PROCEDURE rLoadPromotions
 IS
 vi_PromIdx NUMBER;
 vi_PromItemIdx NUMBER;
 vv_Idx VARCHAR2(500);
 BEGIN
 DBMS_OUTPUT.PUT_LINE ('rLoadPromotions procedure:');
 IF gt_ISPromotionCataloguesLoaded.EXISTS('MainCache') AND gt_ISPromotionCataloguesLoaded('MainCache') <> NVL(TO_CHAR(ctx.gi_ProcInstanceId),'Y') THEN
 DBMS_OUTPUT.PUT_LINE ('rLoadPromotions if _ delete:');
 gt_CatalogPromotions.DELETE;
 gt_CatalogPromotionItems.DELETE;
 gt_CatProm2Items.DELETE;
 gt_CatProm2ItemsCtrlValues.DELETE;
 gt_CatProm2ItemsCtrl.DELETE;
 gt_ISPromotionCataloguesLoaded.DELETE('MainCache');
 gt_DevuelveListaCats.DELETE;
 gt_PromotionCatalogues.DELETE;
 gt_ISPromotionCataloguesLoaded.DELETE;
 END IF;
 --
 IF NOT gt_ISPromotionCataloguesLoaded.EXISTS('MainCache') THEN
 FOR vr IN (SELECT Id,Folder_Id,Cod_Promo,Description,StartPromo
 ,EndPromo,User_Id,Dfrom,Dto,Isvalid,Ctx_Id,Mtx_Id,promo_id
 FROM ( SELECT p.*
 ,ROWNUM OLDTVCableSort
 ,NVL((
 SELECT 'Y'
 FROM y_catalogpromotionsitems i
 WHERE i.catalogpromotions_id = p.Id
 --AND controlname = 'cblSectors'
 AND controlname = 'cblNodo'
 AND ROWNUM <=1
 )
 ,'N') SectorDependentPromo
 FROM y_catalogpromotions p
 --where p.dto is null
 )
 ORDER BY DECODE(SectorDependentPromo,'Y',1,'2'),OLDTVCableSort
 )
 LOOP
 --DBMS_OUTPUT.PUT_LINE ('rLoadPromotions:vr_i :' || vr.id);
 vi_PromIdx := gt_CatalogPromotions.COUNT + 1;
 gt_CatalogPromotions(vi_PromIdx) := vr;
 ---
 FOR vr_i IN (SELECT *
 FROM y_catalogpromotionsitems
 WHERE catalogpromotions_id = vr.Id AND NVL(isvalid,'Y') = 'Y'
 )
 LOOP
 ---
 vi_PromItemIdx := gt_CatalogPromotionItems.COUNT + 1;
 gt_CatalogPromotionItems(vi_PromItemIdx) := vr_i;
 gt_CatProm2Items(vi_PromIdx)(vi_PromItemIdx) := vi_PromItemIdx;
 vv_Idx := vr_i.ControlName||'@'||NVL(TO_CHAR(vr_i.NUMBERValue),vr_i.CharValue);
 gt_CatProm2ItemsCtrlValues(vi_PromIdx)(vv_Idx) := vi_PromItemIdx;
 gt_CatProm2ItemsCtrl(vi_PromIdx)(vr_i.ControlName)(vi_PromItemIdx) := vi_PromItemIdx;
 ---
 END LOOP;
 END LOOP;
 gt_ISPromotionCataloguesLoaded('MainCache') := NVL(TO_CHAR(ctx.gi_ProcInstanceId),'Y');
 END IF;
 END rLoadPromotions;
 /*
 internal procedure using to free cache
 ----------------------------------------------------------------------------------------------------
 */
 PROCEDURE rFreeLookupCache
 IS
 BEGIN
 gt_RTECommonLookups.DELETE;
 END rFreeLookupCache;
 /*
 Internal function used to cache Lookup columns
 ----------------------------------------------------------------------------------------------------
 */
 FUNCTION fiGetColumnLookups (pv_ColumnLookupValue IN VARCHAR2)
 RETURN NUMBER
 IS
 vv_Key cTypes.String := NVL(pv_ColumnLookupValue,cv_NULLValue);
 BEGIN
 RETURN gt_RTEColumnLookups(vv_Key);
 EXCEPTION WHEN NO_DATA_FOUND THEN
 gi_MaxLookupId := gi_MaxLookupId+1;
 gt_RTEColumnLookups(vv_Key) := gi_MaxLookupId;
 ---
 RETURN gt_RTEColumnLookups(vv_Key);
 ---
 END fiGetColumnLookups;
 /*
 Internal function used to cache WHERE clauses
 ----------------------------------------------------------------------------------------------------
 */
 FUNCTION fiGetWhereClausule (pv_WhereClausule IN VARCHAR2)
 RETURN NUMBER
 IS
 vv_Key cTypes.String := NVL(pv_WhereClausule,cv_NULLValue);
 BEGIN
 RETURN gt_RTEWhereClausules(vv_Key);
 EXCEPTION WHEN NO_DATA_FOUND THEN
 gi_MaxLookupId := gi_MaxLookupId+1;
 gt_RTEWhereClausules(vv_Key) := gi_MaxLookupId;
 ---
 RETURN gt_RTEWhereClausules(vv_Key);
 ---
 END fiGetWhereClausule;
 /*
 -- Returns Lookup value from table given in pv_TableName parameter and KeyValue from this table
 -- given in pv_KeyValue parameter. You can also use additional parameter, f. eg. pv_LookupKey if
 -- you do not want use ID as LookupKey or pv_ColumnLookupValue ff do not want use NAME as
 -- ColumnLookupValue
 -- In simple word's: Function cache query result:
 -- SELECT pv_ColumnLookupValue FROM pv_TableName WHERE pv_LookupKey = pv_LookupKey pv_WhereClausule
 -- For example calling this function like that: fvGetLookupValue('tPCProducts',5,'Id','Name','AND IsSubscriptionBased = ''Y''')
 -- cause execute: SELECT Name FROM tPCProducts WHERE Id = 5 AND IsSubscriptionBased = 'Y' , and chage results
 -- If you don't enter parameter pv_LookupKey (default is NAME) or pv_LookupKey - Default is ID, so if you
 -- call this function like that fvGetLookupValue('tPCProducts',5) Product Name will be returned using select:
 -- SELECT Name FROM tPCProducts WHERE Id = 5
 ----------------------------------------------------------------------------------------------------
 */
 FUNCTION fvGetLookupValue(pv_TableName IN VARCHAR2
 ,pv_KeyValue IN VARCHAR2
 ,pv_LookupKey IN VARCHAR2 DEFAULT NULL
 ,pv_ColumnLookupValue IN VARCHAR2 DEFAULT NULL
 ,pv_WhereClausule IN VARCHAR2 DEFAULT NULL
 )
 RETURN VARCHAR2
 IS
 vv_Key cTypes.String;
 vv_Stmt cTypes.String;
 vc_Cur cTypes.CurRef;
 vv_Value1 cTypes.String;
 vv_Value2 cTypes.String;
 BEGIN
 -- calculate lookupvaluekey --
 vv_Key := UPPER(pv_TableName||'@'||pv_LookupKey||'@'||pv_KeyValue)
 ||'@'||fiGetColumnLookups(pv_ColumnLookupValue)
 ||'@'||fiGetWhereClausule(pv_WhereClausule);
 dbms_output.put_line('fvGetLookupValue:vv_Key:'||vv_Key);
 RETURN gt_RTECommonLookups(vv_Key);
 EXCEPTION WHEN NO_DATA_FOUND THEN
 --dbms_output.put_line('fvGetLookupValue:NO_DATA_FOUND');
 IF gt_RTECommonLookups.COUNT >= ci_MaxCommLookupTableSize THEN
 rFreeLookupCache;
 END IF;
 BEGIN
 vv_Stmt := 'SELECT SUBSTR('||NVL(pv_ColumnLookupValue,'lt.Name')||',1,4000)'||CHR(10)||
 'FROM '||pv_TableName||' lt ' ||CHR(10)||
 'WHERE '||NVL(pv_LookupKey,'Lt.Id')||' = :InRELookupKey ' ||CHR(10)||
 pv_WhereClausule;
 OPEN vc_Cur FOR vv_Stmt USING pv_KeyValue;
 FETCH vc_Cur INTO gt_RTECommonLookups(vv_Key);
 CLOSE vc_Cur;
 IF NOT gt_RTECommonLookups.EXISTS(vv_Key) THEN
 gt_RTECommonLookups(vv_Key):= NULL;
 END IF;
 dbms_output.put_line('fvGetLookupValue:NO_DATA_FOUND:vv_Stmt:'||vv_Stmt);
 EXCEPTION WHEN OTHERS THEN
 RETURN '!!! There was an error during Query execution: '||SQLERRM||' for Query : '||vv_Stmt;
 END;
 --dbms_output.put_line('fvGetLookupValue:No_DATA_FOUND:vv_Key:'||vv_Key);
 RETURN gt_RTECommonLookups(vv_Key);
 END fvGetLookupValue;
 FUNCTION fn_queryPromoNew (pn_cparty_id NUMBER
 ,pn_account_id NUMBER
 ,pn_contracteditemd_id NUMBER
 ,pn_Citem_id NUMBER
 ,pd_contractedfrom DATE
 ,pd_dfrom DATE
 ,pv_tipo VARCHAR2
 ,pd_EventDate DATE
 ,pv_parameters1 VARCHAR2
 ,pv_parameters2 VARCHAR2
 ,pn_Amount NUMBER
 ,pv_texto OUT VARCHAR2
 )
 RETURN VARCHAR2
 IS
 vn_retorno1 VARCHAR2 (2000);
 vv_retorno VARCHAR2 (2000);
 vi_SectorId NUMBER;
 vn_cdecoders NUMBER;
 vn_folder_id NUMBER;
 vv_parameters VARCHAR2(2000);
 vi_PIdx NUMBER;
 vi_RegionTypeId NUMBER;
 vi_SectorLevel NUMBER;
 vr_CatProm TRec_CatalogPromotion;
 pos INTEGER;
 pos_inicio INTEGER;
 I INTEGER;
 --vv_param vv_Array_Parameters := vv_Array_Parameters('','','','','','','','','','','','','','','','','','','','','','','','','','');
 vv_param vv_Array_Parameters := vv_Array_Parameters('','','','','','','','','','','','','','','','','','','','','','','','','','','');
 PNN_PROMO_ID NUMBER;
 --::::::PRY_887 Modificacion de Politicas de Cancelacion - IPE
 ln_count_dunning number:=0;
 ld_dunning_min date;
 --:::::::::::::::::::::::::::::::::::::::::::::::::::::
 BEGIN
 vv_parameters := pv_parameters1 || pv_parameters2;
 -- DBMS_OUTPUT.PUT_LINE ('fn_queryPromoNew:');
 pos_inicio := 0;
 For i in 1..26 loop
 pos := instr(vv_parameters,';',1,i);
 if pos = 0 then
 pos := length (vv_parameters) + 1;
 vv_param(i) := substr(vv_parameters,pos_inicio + 1 ,pos-pos_inicio-1);
 --DBMS_OUTPUT.PUT_LINE ('pv_parameters = ' || pv_parameters || 'i = ' || i ||'pos = ' || pos || 'cadena = ' || substr(pv_parameters,pos_inicio + 1 ,pos-pos_inicio-1));
 exit;
 else
 vv_param(i) := substr(vv_parameters,pos_inicio + 1 ,pos-pos_inicio-1);
 --DBMS_OUTPUT.PUT_LINE ('pv_parameters = ' || pv_parameters || 'i = ' || i ||'pos = ' || pos || 'cadena = ' || substr(pv_parameters,pos_inicio + 1,pos-pos_inicio-1));
 pos_inicio := pos;
 end if;
 End Loop;
 Begin
 select folder_id
 into vn_folder_id
 from tpcproducts
 where id = vv_param(12);
 Exception When Others Then
 raise_application_error (-20000,' Error en TPCPRODUCTS : ' || vv_param(12));
 end;
 vv_param(27):= vn_folder_id;
 vn_cdecoders:=NVL(vv_param(19),vv_param(25));
 /* -------------------------------------------------------------------------------------------*/
 -- Get region based on Citem data:
 -- a) on EventDate - full expression from promotion package:
 -- " NVL(NVL(pd_EventDate,pd_dfrom),pd_contractedfrom) "
 -- b) For region type (mean hierarchy type) configured in "General Directory"
 -- in List Of Values with symbol : "RegionDPSComponentConfig" in items called:
 -- * Default hierarchy (used in old BSCC.YPKG_PROMOCIONES mechanism)
 -- now is: "Geographic areas for pricing" - Name of the hierarchy types should be entered
 -- * Default hierarchy level (used in old YPKG_PROMOCIONES mechanism - as sectors)
 -- now is: "5" - based on existing test configuration, should be changed
 -- on: REAL HIERAARCHY LEVEL OF THE SECTORS
 -- c) Based on addresses in following priority:
 -- * API - First from Location address from access point: (tAMApInstanceD.LocationAddress_Id)
 -- * ACC - Account (tAMCpartyAccounD.PernamentAddress_Id) in second step - only in case when
 -- API has no retured Region (can be for example: citem has no access point on is
 -- lack of address for access point )
 -- * CP - Contractting party (tAMContractingPartyD.PernamentAddress_Id) in third step
 -- only in case when API and ACC has no retured Region (should be this case beacuse
 -- Account have an address)
 /* -------------------------------------------------------------------------------------------*/
 vi_RegionTypeId := fvGetLookupValue('tHSHierarchyTypes'
 ,fvGetLookupValue('tREPValueListItems'
 ,'RegionDPSComponentConfig'
 ,'ValueListSymbol'
 ,'AValue'
 ,' AND BValue = ''Default_Hierarchy'''
 )
 ,'Name','Id');
 vi_SectorLevel := fvGetLookupValue('tREPValueListItems'
 ,'RegionDPSComponentConfig'
 ,'ValueListSymbol'
 ,'AValue'
 ,' AND BValue = ''Default_Hierarchy_level'''
 );

 vi_SectorId := xpDPSTools.fiGetPricingRegion
 (pi_citemid => pn_Citem_id
 ,pi_regiontypeid => vi_RegionTypeId
 ,pd_date => NVL(NVL(pd_EventDate,pd_dfrom),pd_contractedfrom)
 ,pv_addresspriority => 'API,ACC,CP'
 ,pi_regionlevel => vi_SectorLevel
 ,pi_regionidtocompare => NULL
 ,pv_comparisionmode => NULL
 );
 ----------------
 rLoadPromotions;
 ----------------
 --DBMS_OUTPUT.PUT_LINE ('rLoadPromotions:');
 vi_PIdx := gt_CatalogPromotions.FIRST;
 --:::::::::::::::PRY_887 - IPE:::::::::::::::::
 /*begin
  begin
   select min(opendate)
            into ld_dunning_min
                from TDUNPROCEEDINGS t
                WHERE t.cparty_id = pn_cparty_id
               and t.CPARTYACCOUNT_ID = pn_account_id;
    exception
     when no_data_found then
      ld_dunning_min:=pd_EventDate;
   end;
  if nvl(ld_dunning_min,pd_EventDate) <= pd_EventDate then
    select count(1) into ln_count_dunning
      from TAMCONTRACTEDITEMD a
     where a.CITEMDCITEM_ID = pn_Citem_id
       and state = 'S'
       and isvalid = 'Y'
       and exists (select 1
                     from TDUNPROCEEDINGS t
                    where t.cparty_id = a.cparty_id
                      and t.CPARTYACCOUNT_ID = a.CPARTYACCOUNT_ID
                      and a.dfrom BETWEEN t.opendate and nvl(t.closedate,a.dfrom));
   end if;
 end;
 if ln_count_dunning = 0 then*/
 --::::::::::::::::::::::::::::::::::::::::::::::
 LOOP
 --------------------------
 EXIT WHEN vi_PIdx IS NULL;
 --------------------------
 vr_CatProm := gt_CatalogPromotions(vi_PIdx);
 DBMS_OUTPUT.PUT_LINE ('vr_CatProm.ID :' || vr_CatProm.ID);
 -- DBMS_OUTPUT.PUT_LINE ('vr_CatProm.startpromo :' || vr_CatProm.startpromo);
 -- DBMS_OUTPUT.PUT_LINE ('vr_CatProm.endpromo :' || vr_CatProm.endpromo);
 --Consulta principal, verifica si un citem tiene una promocion
 IF pd_contractedfrom BETWEEN vr_CatProm.startpromo AND NVL (vr_CatProm.endpromo, pd_contractedfrom)
 and pd_EventDate BETWEEN vr_CatProm.dfrom AND NVL (vr_CatProm.dto, pd_EventDate)
 AND YPKG_PROMOCIONES.fvGetLookupValue
 ('(SELECT 1 Id, count(1) Name ' ||CHR(10)||
 ' FROM y_catalogpromotionsitems i ' ||CHR(10)||
 ' WHERE i.catalogpromotions_id='||vr_CatProm.ID ||CHR(10)||
 ' AND controlname = ''cmbProduct'' ' ||CHR(10)||
 ' AND NUMBERVALUE = '||NVL(VV_PARAM(12),'NULL')||CHR(10)|| --product_id
 ' and rownum<2' ||CHR(10)||
 ')'
 ,1) > 0
 AND YPKG_PROMOCIONES.fvGetLookupValue
 ('(SELECT 1 Id, count(1) Name ' ||CHR(10)||
 ' FROM y_catalogpromotionsitems i ' ||CHR(10)||
 ' WHERE i.catalogpromotions_id='||vr_CatProm.ID ||CHR(10)||
 ' AND controlname = ''cblCiudad'' ' ||CHR(10)||
 ' AND NUMBERVALUE = '||NVL(vv_param(9),'NULL')||CHR(10)|| --Cuidad
 ' and rownum<2' ||CHR(10)||
 ')'
 ,1) > 0
 AND ( YPKG_PROMOCIONES.fvGetLookupValue
 ('(SELECT 1 Id, count(1) Name ' ||CHR(10)||
 ' FROM y_catalogpromotionsitems i ' ||CHR(10)||
                      '  WHERE i.catalogpromotions_id='||vr_CatProm.ID ||CHR(10)||
                      '    AND controlname = ''cblFormaPago''  '       ||CHR(10)||
                      '    AND NUMBERVALUE = '||NVL(vv_param(6),'NULL')||CHR(10)|| --Forma de Pago
                      '    and rownum<2'                               ||CHR(10)||
                      ')'
                     ,1) IN (0,1)
                 -- Remark !!!: this condition is to invasigate by TV Cable because with one
                 -- condition before give no differ effort
                )
            AND (  YPKG_PROMOCIONES.fvGetLookupValue
                     ('(SELECT 1 Id, count(1) Name           '                            ||CHR(10)||
                       '   FROM y_catalogpromotionsitems i   '                            ||CHR(10)||
                       '  WHERE i.catalogpromotions_id='||vr_CatProm.ID                   ||CHR(10)||
                       '    AND controlname = ''cblNegocio''  '                           ||CHR(10)||
                       '    AND NUMBERVALUE = '||NVL(NVL(VV_PARAM(18),VV_PARAM(8)),'NULL')||CHR(10)|| --Negocio
                       '    and rownum<2'                                                 ||CHR(10)||
                       ')'
                     ,1) IN (0,1)
                 -- Remark !!!: this condition is to invasigate by TV Cable because with one
                 -- condition before give no differ effort
                )
            AND (  (pv_tipo = 'M'
                       AND YPKG_PROMOCIONES.fvGetLookupValue
                             ('(SELECT 1 Id, count(1) Name           '          ||CHR(10)||
                              '   FROM y_catalogpromotionsitems i   '           ||CHR(10)||
                              '  WHERE i.catalogpromotions_id='||vr_CatProm.ID  ||CHR(10)||
                              '    AND controlname = ''txtServicio''  '         ||CHR(10)||
                              '    AND NUMBERVALUE IS NOT NULL '                ||CHR(10)|| --Servicio
                              '    and rownum<2'                                ||CHR(10)||
                              ')'
                              ,1) > 0
                    )
                 OR (pv_tipo = 'I'
                      AND YPKG_PROMOCIONES.fvGetLookupValue
                             ('(SELECT 1 Id, count(1) Name           '         ||CHR(10)||
                              '   FROM y_catalogpromotionsitems i   '          ||CHR(10)||
                              '  WHERE i.catalogpromotions_id='||vr_CatProm.ID ||CHR(10)||
                              '    AND controlname in(''txtInstalacion'',''txtinstalacionHD'')  '     ||CHR(10)||
                              '    AND NUMBERVALUE IS NOT NULL '               ||CHR(10)|| --Instalacion
                              '    and rownum<2'                               ||CHR(10)||
                              ')'
                              ,1) > 0
                    )
                )
            --AND ( /*1. Promotion which are configured as SECTOR DEPENDEN - mean: HAS ANY items in (y_catalogpromotionsitems)  with controlname = 'cblSectors */
            --                  (   YPKG_PROMOCIONES.fvGetLookupValue
            --                          ('(SELECT 1 Id, count(1) Name'                                   ||CHR(10)||
            --                          '   FROM y_catalogpromotionsitems i'                             ||CHR(10)||
            --                          '  WHERE i.catalogpromotions_id='||vr_CatProm.ID                 ||CHR(10)||
            --                          '    AND controlname = ''cblSectors'' '                          ||CHR(10)||
            --                          '    and rownum<2'                                               ||CHR(10)||
            --                          ')'
            --                         ,1) > 0
            /* 1.1 Should  have Sector equals with items or item  - NUMBERVALUE can be NULL - but cblSectors - must to be defined*/
            --AND YPKG_PROMOCIONES.fvGetLookupValue
            --                           ('(SELECT 1 Id, count(1) Name          '                   ||CHR(10)||
            --                            '   FROM y_catalogpromotionsitems i   '                   ||CHR(10)||
            --                            '  WHERE i.catalogpromotions_id='||vr_CatProm.ID          ||CHR(10)||
            --                            '    AND controlname = ''cblSectors'' '                   ||CHR(10)||
            --                            '    AND NUMBERVALUE = '||NVL(TO_CHAR(vi_SectorId),'NULL')||CHR(10)||
            --                            '    and rownum<2'                                        ||CHR(10)||
            --                            ')'
            --                           ,1) > 0
            --                   )
            -- OR ( /*2. Promotion which are configured as NO SECTOR DEPENDEN - mean:HAS NO items in (y_catalogpromotionsitems) with controlname = 'cblSectors */
            -- YPKG_PROMOCIONES.fvGetLookupValue
            --                        ('(SELECT 1 Id, count(1) Name           '          ||CHR(10)||
            --                         '   FROM y_catalogpromotionsitems i   '           ||CHR(10)||
            --                         '  WHERE i.catalogpromotions_id='||vr_CatProm.ID  ||CHR(10)||
            --                         '    AND controlname = ''cblSectors'' '           ||CHR(10)||
            --                         '    and rownum<2'                                ||CHR(10)||
            --                         ')'
            --                       ,1) = 0
            --                   )
            --                )
      THEN
        --Entra a la Funcion que va a curzar los datos para aplicar la promocion y retorna el valor y el texto
        DBMS_OUTPUT.put_line ('Entraalif:fn_queryPromoNew:vr_CatProm.Id:'||vr_CatProm.Id);
        vv_retorno := fv_Get_Promo(pn_promo_id           => vr_CatProm.Id
                                   ,pn_cparty            => pn_cparty_id
                                   ,pn_account           => pn_account_id
                                   ,pn_citem             => pn_citem_id
                                   ,pd_eventdate         => pd_EventDate
                                   ,pn_contracteditemd_id=> pn_contracteditemd_id
                                   ,pd_contractedfrom    => pd_contractedfrom
                                   ,Pn_Amount            => pn_Amount
                                   ,pv_parameters        => vv_param
                                   ,vv_tipo              => pv_tipo
                                  );
        --Si hay algun valor significa que tiene promocion, entonces graba en tabla de log
        --Si encuentra una promocion activa sale
        pnn_promo_id :=vr_CatProm.Id;
        ----------------------------------
        EXIT WHEN vv_retorno IS NOT NULL;
        DBMS_OUTPUT.put_line ('fn_queryPromoNew:vn_retorno_IS NOT NULL:' || vv_retorno);
        ----------------------------------
      END IF;
      vi_PIdx := gt_CatalogPromotions.NEXT(vi_PIdx);
    END LOOP;
  --end if;
    --Caso contrario, si no hay promocion saca los valores por default
    IF vv_retorno IS NULL THEN
      vv_retorno := pn_Amount || ';';
      DBMS_OUTPUT.put_line ('fn_queryPromoNew:ValoresDefault:vv_retorno:'||vv_retorno);
    END IF;
    --Variable que tendra el valor a cobrarse
    IF vv_retorno IS NOT NULL THEN
      vn_retorno1 :=TO_CHAR (SUBSTR (vv_retorno, 0, INSTR (vv_retorno, ';') - 1));
      pv_texto :=SUBSTR (vv_retorno,
                      INSTR (vv_retorno, ';') + 1,
                      LENGTH (vv_retorno)
                     );
      DBMS_OUTPUT.put_line ('fn_queryPromoNew:ValoresCalculados:vn_retorno1:'||vn_retorno1);
    ELSE
      vn_retorno1 := 0;
      pv_texto := NULL;
    END IF;
    IF vn_folder_id = 2 THEN
      DBMS_OUTPUT.put_line ('fn_queryPromoNew:vn_folder_id:'||vn_folder_id||':vn_cdecoders:'||vn_cdecoders||':pv_tipo:'||pv_tipo);
      IF vn_cdecoders = 1 AND pv_tipo = 'M' THEN
        pv_texto := 'Principal ' || pv_texto;
        ELSIF vn_cdecoders = 1 AND pv_tipo = 'I' THEN
          pv_texto := 'Instalacion ' || pv_texto;
        ELSIF vn_cdecoders > 1 AND pv_tipo = 'M' THEN
          pv_texto := 'Adicional(es) ' || pv_texto;
      END IF;
    ELSE
      IF pv_tipo = 'I' THEN
        pv_texto := 'Instalacion ' || pv_texto;
      END IF;
    END IF;
    DBMS_OUTPUT.put_line ('FIN_fn_queryPromoNew:vn_retorno1:'||vn_retorno1||':pv_texto:'||pv_texto);
    RETURN vn_retorno1;
  END fn_queryPromoNew;
  FUNCTION fn_queryPromo
    (
      pn_cparty_id          NUMBER,
      pn_account_id         NUMBER,
      pn_contracteditemd_id NUMBER,
      pn_Citem_id           NUMBER,
      pd_contractedfrom     DATE,
      pd_dfrom              DATE,
      pv_tipo               VARCHAR2,
      pd_EventDate          DATE,
      pv_parameters1        VARCHAR2,
      pv_parameters2        VARCHAR2,
      pn_Amount             NUMBER,
      pv_texto              OUT VARCHAR2
    )
  RETURN VARCHAR2
  IS
    vn_retorno1   VARCHAR2 (2000);
    vv_retorno    VARCHAR2 (2000);
    vi_SectorId   NUMBER;
    vn_cdecoders  NUMBER;
    vn_folder_id  NUMBER;
    vv_parameters VARCHAR2(2000);
    vi_PIdx       NUMBER;
    vr_CatProm    TRec_CatalogPromotion;
    pos           INTEGER;
    pos_inicio    INTEGER;
    I             INTEGER;
    --vv_param vv_Array_Parameters := vv_Array_Parameters('','','','','','','','','','','','','','','','','','','','','','','','','','')
    vv_param vv_Array_Parameters := vv_Array_Parameters('','','','','','','','','','','','','','','','','','','','','','','','','','','');
    PNN_PROMO_ID NUMBER;
  BEGIN
    DBMS_OUTPUT.PUT_LINE ('fn_queryPromo');
    RETURN fn_queryPromoNew ( pn_cparty_id            => pn_cparty_id
                              ,pn_account_id          => pn_account_id
                              ,pn_contracteditemd_id  => pn_contracteditemd_id
                              ,pn_Citem_id            => pn_Citem_id
                              ,pd_contractedfrom      => pd_contractedfrom
                              ,pd_dfrom               => pd_dfrom
                              ,pv_tipo                => pv_tipo
                              ,pd_EventDate           => pd_EventDate
                              ,pv_parameters1         => pv_parameters1
                              ,pv_parameters2         => pv_parameters2
                              ,pn_Amount              => pn_Amount
                              ,pv_texto               => pv_texto
                            );
    vv_parameters := pv_parameters1 || pv_parameters2;
    -- DBMS_OUTPUT.PUT_LINE ('vv_parameters:'||pv_parameters1 || pv_parameters2);
    pos_inicio := 0;
    For i in 1..26 loop
      pos := instr(vv_parameters,';',1,i);
      if pos = 0 then
        pos := length (vv_parameters) + 1;
        vv_param(i) := substr(vv_parameters,pos_inicio + 1 ,pos-pos_inicio-1);
        --DBMS_OUTPUT.PUT_LINE ('pv_parameters = ' || pv_parameters || 'i = ' || i ||'pos = ' || pos || 'cadena = ' || substr(pv_parameters,pos_inicio + 1 ,pos-pos_inicio-1));
        exit;
      else
        vv_param(i) := substr(vv_parameters,pos_inicio + 1 ,pos-pos_inicio-1);
        --DBMS_OUTPUT.PUT_LINE ('pv_parameters = ' || pv_parameters || 'i = ' || i ||'pos = ' || pos || 'cadena = ' || substr(pv_parameters,pos_inicio + 1,pos-pos_inicio-1));
        pos_inicio := pos;
      end if;
    End Loop;
    begin
      select folder_id
      into vn_folder_id
      from tpcproducts
      where id = vv_param(12);
    exception when others then
      raise_application_error (-20000,' Error en TPCPRODUCTS : ' || vv_param(12));
    end;
    vv_param(27):= vn_folder_id;
    vn_cdecoders:=NVL(vv_param(19),vv_param(25));
    --Consulta principal, verifica si un citem tiene una promocion
    FOR cur IN
           ( SELECT y.ID promo_id
             FROM  y_catalogpromotions y
             WHERE pd_contractedfrom BETWEEN y.startpromo AND NVL (y.endpromo, pd_contractedfrom)
             AND (  SELECT count(1)
                    FROM y_catalogpromotionsitems i
                    WHERE y.ID = i.catalogpromotions_id
                    AND controlname = 'cmbProduct'
                    AND NUMBERVALUE = VV_PARAM(12) --product_id
                    AND rownum<2
                 ) > 0
             AND (  SELECT count(1)
                    FROM y_catalogpromotionsitems i
                    WHERE y.ID = i.catalogpromotions_id
                    AND controlname = 'cblCiudad'
                    AND NUMBERvalue = vv_param(9) --costcenter_id
                    and rownum<2
                 ) > 0
             AND (
                    ( SELECT count(1)
                      FROM y_catalogpromotionsitems i
                      WHERE y.ID = i.catalogpromotions_id
                      AND CONTROLNAME = 'cblFormaPago'
                      AND NUMBERVALUE = VV_PARAM(6) --Forma de Pago
                      and rownum<2
                     ) > 0
                 OR
                  NOT EXISTS (  SELECT 1
                                FROM y_catalogpromotionsitems i
                                WHERE y.ID = i.catalogpromotions_id
                                AND controlname = 'cblFormaPago'
                                AND NUMBERvalue = vv_param(6) --Forma de Pago
                             )
                 )
             AND (
                  ( SELECT count(1)
                    FROM y_catalogpromotionsitems i
                    WHERE y.ID = i.catalogpromotions_id
                    AND controlname = 'cblNegocio'
                    AND NUMBERVALUE = NVL(VV_PARAM(18),VV_PARAM(8)) --Business o Negocio
                    AND rownum<2
                  ) > 0
                 or
                  not exists (  SELECT 1
                                FROM y_catalogpromotionsitems i
                                WHERE y.ID = i.catalogpromotions_id
                                AND controlname = 'cblNegocio'
                                AND NUMBERvalue = nvl(vv_param(18),vv_param(8)) --Business o Negocio
                             )
                 )
             AND (
                  ( pv_tipo = 'M'
                    AND ( SELECT count(1)
                          FROM y_catalogpromotionsitems i
                          WHERE y.ID = i.catalogpromotions_id
                          AND controlname = 'txtServicio'
                          AND NUMBERVALUE IS NOT NULL -- Valor descuento en Servicio
                          and rownum<2
                        ) > 0
                  )
                 or ( pv_tipo = 'I'
                      AND ( SELECT count(1)
                            FROM y_catalogpromotionsitems i
                            WHERE y.ID = i.catalogpromotions_id
                            AND controlname in ('txtInstalacion','txtInstalacionHD')
                            AND NUMBERVALUE IS NOT NULL -- Valor descuento en Instalacion
                            AND rownum<2
                          ) > 0)
                    )
           )
    LOOP
      DBMS_OUTPUT.put_line ('fn_queryPromo:Entra_For:cur.curpromo:' || cur.promo_id);
      --Entra a la Funcion que va a curzar los datos para aplicar la promocion y retorna el valor y el texto
      vv_retorno := fv_Get_Promo(
          pn_promo_id           => cur.promo_id,
          pn_cparty             => pn_cparty_id,
          pn_account            => pn_account_id,
          pn_citem              => pn_citem_id,
          pd_eventdate          => pd_EventDate,
          pn_contracteditemd_id => pn_contracteditemd_id,
          pd_contractedfrom     => pd_contractedfrom,
          Pn_Amount             => pn_Amount,
          pv_parameters         => vv_param,
          vv_tipo               => pv_tipo
          );
      --Si hay algun valor significa que tiene promocion, entonces graba en tabla de log
      --Si encuentra una promocion activa sale
      pnn_promo_id :=cur.promo_id;
      EXIT WHEN vv_retorno IS NOT NULL;
      DBMS_OUTPUT.put_line ('fn_queryPromo:pnn_promo_id_IS NOT NULL:'|| pnn_promo_id);
    END LOOP;
    --Caso contrario, si no hay promocion saca los valores por default
    DBMS_OUTPUT.put_line ('fn_queryPromo:vv_retorno IS NULL:'|| vv_retorno);
    IF vv_retorno IS NULL THEN
      vv_retorno := pn_Amount || ';';
      DBMS_OUTPUT.put_line ('fn_queryPromo:Default:' || vv_retorno);
    END IF;
    --Variable que tendra el valor a cobrarse
    IF vv_retorno IS NOT NULL THEN
      vn_retorno1 :=TO_CHAR (SUBSTR (vv_retorno, 0, INSTR (vv_retorno, ';') - 1));
      pv_texto :=SUBSTR (vv_retorno,
                 INSTR (vv_retorno, ';') + 1,
                 LENGTH (vv_retorno)
                 );
      DBMS_OUTPUT.put_line ('fn_queryPromo:Calculado:' || vv_retorno);
    ELSE
      vn_retorno1 := 0;
      pv_texto := NULL;
    END IF;
    IF vn_folder_id = 2 THEN
      --DBMS_OUTPUT.put_line ('fn_queryPromo:vn_folder_id:' || vn_folder_id||':vn_cdecoders:'||vn_cdecoders||':pv_tipo:'||pv_tipo);
      IF vn_cdecoders = 1 AND pv_tipo = 'M' THEN
        pv_texto := 'Principal ' || pv_texto;
      ELSIF vn_cdecoders = 1 AND pv_tipo = 'I' THEN
        pv_texto := 'Instalacion ' || pv_texto;
      ELSIF vn_cdecoders > 1 AND pv_tipo = 'M' THEN
        pv_texto := 'Adicional(es) ' || pv_texto;
      END IF;
    ELSE
      IF pv_tipo = 'I' THEN
        pv_texto := 'Instalacion ' || pv_texto;
      END IF;
    END IF;
    DBMS_OUTPUT.put_line ('fn_queryPromoFin:'||vn_retorno1);
    RETURN vn_retorno1;
  END fn_queryPromo;
  FUNCTION fv_devuelveListaCat (vn_idPromocion NUMBER, vv_controlname VARCHAR2) return VARCHAR2 is
    vv_Key    VARCHAR2(32767):=NVL(vn_idPromocion,0)||'@'||vv_controlname;
    vv_dato   VARCHAR2(32767):='';
  begin
    RETURN gt_DevuelveListaCats(vv_Key);
    dbms_output.put_line(' from cache');
  EXCEPTION WHEN NO_DATA_FOUND THEN
    for cur in (
                select distinct t1.catalogpromotions_id, t1.controlname, t1.NUMBERvalue, t1.dfrom, t1.proyecto_id
                from y_catalogpromotionsitems t1
                where t1.catalogpromotions_id = vn_idPromocion
                and t1.controlname = vv_controlname
                order by t1.NUMBERvalue
               )
    loop
      vv_dato := vv_dato || '|' || cur.NUMBERvalue;
    end loop;
      vv_dato := vv_dato || '|';
      gt_DevuelveListaCats(vv_Key) := vv_dato;
      dbms_output.put_line(' not from cache');
    return vv_dato;
  END fv_devuelveListaCat;

  FUNCTION fv_devuelveListaCatCached (vn_idPromocion NUMBER, vv_controlname VARCHAR2) return VARCHAR2 is
    vv_Key    VARCHAR2(32767):=NVL(vn_idPromocion,0)||'@'||vv_controlname;
    vv_dato   VARCHAR2(32767):='';
  begin
    RETURN gt_DevuelveListaCats(vv_Key);
  EXCEPTION WHEN NO_DATA_FOUND THEN
    gt_DevuelveListaCats(vv_Key):=NULL;
    return NULL;
  END fv_devuelveListaCatCached;


  PROCEDURE rReLoadDevuelveListaCat
  IS
    vv_Key    VARCHAR2(32767);
    vv_dato   VARCHAR2(32767):='';
  BEGIN
    gt_DevuelveListaCats.DELETE;
    FOR vr IN (SELECT DISTINCT T1.CATALOGPROMOTIONS_ID, T1.CONTROLNAME, T1.NUMBERVALUE, T1.DFROM, T1.PROYECTO_ID
                 FROM Y_CATALOGPROMOTIONSITEMS T1
               ORDER BY T1.CATALOGPROMOTIONS_ID,T1.CONTROLNAME,T1.NUMBERVALUE
              )
    LOOP
       vv_Key := NVL(vr.CATALOGPROMOTIONS_ID,0)||'@'||vr.CONTROLNAME;
       IF NOT gt_DevuelveListaCats.EXISTS(vv_Key) THEN
          gt_DevuelveListaCats(vv_Key) := '|'||vr.NumberValue||'|';
       ELSE
          gt_DevuelveListaCats(vv_Key) := gt_DevuelveListaCats(vv_Key)||vr.NumberValue||'|';
       END IF;
    END LOOP;
  END;

  FUNCTION fv_Get_Promo
    (
      pn_promo_id           NUMBER,
      pn_cparty             NUMBER,
      pn_account            NUMBER,
      pn_citem              NUMBER,
      pd_eventdate          DATE,
      pn_contracteditemd_id NUMBER,
      pd_contractedfrom     DATE,
      pn_Amount             NUMBER,
      pv_parameters         vv_Array_Parameters,
      vv_tipo               VARCHAR2
    )
  RETURN VARCHAR2
  IS
    vn_resultado        NUMBER := 1;
    vn_precio           NUMBER := 0;
    vn_resultadofinal2  VARCHAR2 (200);
    var1                NUMBER;
    var2                VARCHAR2 (200);
    vn_respuesta        VARCHAR2 (200);
    vn_instdiferida     NUMBER;
    vn_bValue           INTEGER;
    Vn_Percent          NUMBER;
    Vi_mes              INTEGER;
    vv_param            vv_Array_Parameters;
    Vi_IsContractedInPI INTEGER;
    vi_contador         INTEGER;
    cur                 TRec_PromotionCatalogue;
    vi_folder_id        INTEGER;
    vn_isprincipal      NUMBER;
    vn_isprincipalNBHD  NUMBER;
    vf_activ            DATE;
  BEGIN
    vn_instdiferida:=1;
    DBMS_OUTPUT.PUT_LINE ('fv_Get_Promo');
    vv_param := pv_parameters;
    vi_folder_id := vv_param(27);
    vn_resultado := 1;
    -- Load catalogue to cache
    rLoadPromotionCatalogue;
    -- returns when no records in catalogue
    /*DBMS_OUTPUT.PUT_LINE ('vv_param:'||vv_param(1)||':'||vv_param(2)||':'||vv_param(3)||':'||vv_param(4)||':'||vv_param(5)||':'||vv_param(6)||':'||vv_param(7)||':'||vv_param(8)||':'||vv_param(9)
    ||':'||vv_param(10)||':'||vv_param(11)||':'||vv_param(12)||':'||vv_param(13)||':'||vv_param(14)||':'||vv_param(15)||':'||vv_param(16)||':'||vv_param(17)||':'||vv_param(18)
    ||':'||vv_param(19)||':'||vv_param(20)||':'||vv_param(21)||':'||vv_param(22)||':'||vv_param(23)||':'||vv_param(24)||':'||vv_param(25)||':'||vv_param(26)||':'||vv_param(27)||':'||vv_tipo);
    */
     --Verificacion de fecha de activacion del producto
     vf_activ:= YBSIMPLEMENTACION.FDINSTALLATIONDATE (pn_citem);
    IF NOT gt_PromotionCatalogues.EXISTS(pn_promo_id) AND gt_ISPromotionCataloguesLoaded.EXISTS(pn_promo_id) THEN
       NULL; -- do nothing
    ELSE
      -- for througth catalogue --
      FOR vr IN NVL(gt_PromotionCatalogues(pn_promo_id).FIRST,1) .. NVL(gt_PromotionCatalogues(pn_promo_id).LAST,0)
      LOOP
        cur := gt_PromotionCatalogues(pn_promo_id)(vr);
        --TRec_PromotionCatalogue
        DBMS_OUTPUT.PUT_LINE ('cur.controlname = ' || cur.controlname || ', pn_promo_id = ' || pn_promo_id || ', vv_param(9) = ' || vv_param(9) || ', cur.NUMBERvalue = ' || cur.NUMBERvalue);
        case
          when cur.controlname = 'cmbProduct' and not(instr(cur.NUMBERvalue,'|' || vv_param(12) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cmbProduct:'||vv_param(12));exit;
          when cur.controlname = 'cblCalificacionCliente' and not(instr(cur.NUMBERvalue,'|' || vv_param(3) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblCalificacionCliente:'||vv_param(3));exit;
          when cur.controlname = 'cblNodo' and not(instr(cur.NUMBERvalue,'|' || vv_param(23) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblNodo:'||vv_param(23));exit;
          when cur.controlname = 'fldFolder' and not(instr(cur.NUMBERvalue,'|' || vi_folder_id || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por fldFolder:'||vi_folder_id);exit;
          when cur.controlname = 'cblPlan' and not(instr(cur.NUMBERvalue,'|' || vv_param(14) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblPlan:'||vv_param(14));exit;
          when cur.controlname = 'cblNegocio' and not(instr(cur.NUMBERvalue,'|' || nvl(vv_param(18),vv_param(8)) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblNegocio:'||vv_param(18)||':'||vv_param(8));exit;
          when cur.controlname = 'cblFormaPago' and not(instr(cur.NUMBERvalue,'|' || vv_param(17) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblFormaPago:'||vv_param(17));exit;
          when cur.controlname = 'cblCiudad' and not(instr(cur.NUMBERvalue,'|' || vv_param(9) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblCiudad:'||vv_param(6));exit;
          when cur.controlname = 'cbltipodecodificador' and not(instr(cur.NUMBERvalue,'|' || vv_param(24) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cbltipodecodificador:'||vv_param(24));exit;
          when cur.controlname = 'cblTipoCuenta' and not(instr(cur.NUMBERvalue,'|' || vv_param(5) || '|',1) > 0)  THEN vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoCuenta:'||vv_param(5));exit;
          -- when cur.controlname = 'cblTipoServicio' AND vi_folder_id = 2 and nvl(vv_param(24),0) = 503950 then
          when cur.controlname = 'cblTipoServicio' and vi_folder_id = 2 then -- and (vv_tipo='M' or (vv_tipo='I' and  nvl(vv_param(24),0) in (503950))) then
            DBMS_OUTPUT.PUT_LINE ('cur.controlname:'||cur.controlname||':cblTipoServicio:'||vi_folder_id);
            IF(vv_param(12)=153) THEN
              DBMS_OUTPUT.PUT_LINE ('Producto:NBHD:vv_param(12):'||vv_param(12));
              Begin
                vn_isprincipalNBHD:=ypkg_promociones.fn_buscaprincipalNBHD(vv_param(1),vv_param(4),vv_param(10),vv_param(11));
                IF (vn_isprincipalNBHD=1) THEN
                  SELECT  id
                  INTO vn_bValue
                  FROM trepvaluelistitems
                  WHERE VALUELISTSYMBOL = 'TVC_CPP'
                  and avalue = 'cblTipoServicio'
                  and bvalue = vn_isprincipalNBHD;
                  DBMS_OUTPUT.PUT_LINE ('vn_isprincipalNBHD:vn_isprincipalNBHD:'||vn_isprincipal||':vn_bValue:'||vn_bValue);
                ELSE
                  SELECT  id
                  INTO vn_bValue
                  FROM trepvaluelistitems
                  WHERE VALUELISTSYMBOL = 'TVC_CPP'
                  and avalue = 'cblTipoServicio'
                  and bvalue = decode(vv_param(19),0,vv_param(25)
                                                    ,vv_param(19));
                  DBMS_OUTPUT.PUT_LINE ('vn_isprincipalNBHD:vn_isprincipalNBHD:'||vn_isprincipalNBHD||':vn_bValue:'||vn_bValue);
                END IF;
                if not(instr(cur.numbervalue,'|' || vn_bValue || '|',1) > 0 ) THEN
                   vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoServicioNBHD:'||vv_param(25));exit;
                end if;
              End;
            ELSE
              Begin
                SELECT  id
                INTO vn_bValue
                FROM trepvaluelistitems
                WHERE VALUELISTSYMBOL = 'TVC_CPP'
                and avalue = 'cblTipoServicio'
                and bvalue = decode(vv_param(19),0,vv_param(25)
                                                  ,vv_param(19));
                DBMS_OUTPUT.PUT_LINE ('vn_bValue:'||vn_bValue||':vv_param(19),vv_param(25):'||vv_param(19)||':'||vv_param(25));
              EXCEPTION WHEN no_data_found THEN
                vn_bValue := 0;DBMS_OUTPUT.PUT_LINE ('excepcion por cblTipoServicio:'||vv_param(19));
              End;
              if not(instr(cur.numbervalue,'|' || vn_bValue || '|',1) > 0 ) THEN
                vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoServicio:'||vv_param(19));exit;
              end if;
            END IF;
          when cur.controlname = 'cblTipoServicioHD' AND vi_folder_id = 2 and nvl(vv_param(24),0)!= 503950 and vv_tipo='I'  then
            Begin
              vn_isprincipal:=YEXPRESSIONRULES.fn_buscaprincipalhd_1(vv_param(1),vv_param(4),vv_param(10));
              IF (vn_isprincipal=1) THEN
                SELECT  id
                INTO vn_bValue
                FROM trepvaluelistitems
                WHERE VALUELISTSYMBOL = 'TVC_CPP'
                and avalue = 'cblTipoServicio'
                and bvalue = vn_isprincipal;
                DBMS_OUTPUT.PUT_LINE ('cblTipoServicioHD:vn_isprincipal:'||vn_isprincipal||':vn_bValue:'||vn_bValue);
              ELSE
                SELECT  id
                INTO vn_bValue
                FROM trepvaluelistitems
                WHERE VALUELISTSYMBOL = 'TVC_CPP'
                and avalue = 'cblTipoServicio'
                and bvalue = vv_param(25);
                DBMS_OUTPUT.PUT_LINE ('cblTipoServicioHD:vn_isprincipal:'||vn_isprincipal||':vn_bValue:'||vn_bValue);
              END IF;
              if not(instr(cur.NUMBERvalue,'|' || vn_bValue || '|',1) > 0 ) THEN
                vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoServicioHD:'||vv_param(25));exit;
              end if;
            End;
          when cur.controlname = 'cblTipoServicio' AND vi_folder_id != 2 and NOT (instr(cur.NUMBERvalue,'|' || 505956 || '|',1) > 0) THEN
            vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoServicio folder');exit;
          when cur.controlname = 'cblMesesPromocion' then
            Begin
              select (EXTRACT(month FROM pd_eventdate)+EXTRACT(year FROM pd_eventdate)*12) - (EXTRACT(month FROM vf_activ)+EXTRACT(year FROM vf_activ)*12)
              into Vi_mes
              from dual;
              if  NOT (instr(cur.NUMBERvalue,'|' || Vi_mes || '|',1) > 0) THEN
                vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblMesesPromocion:'||cur.NUMBERvalue||';Vi_mes '||Vi_mes||';'||vn_resultado);exit ;
              End If;
            End;
          when cur.controlname = 'cmbMesesDifInstalacion' then
            begin
              select (EXTRACT(month FROM pd_eventdate)+EXTRACT(year FROM pd_eventdate)*12) - (EXTRACT(month FROM vf_activ)+EXTRACT(year FROM vf_activ)*12)+1
              into Vi_mes
              from dual;
              select length(cur.NUMBERvalue) - length(replace(cur.NUMBERvalue,'|','')) - 1
              into vn_instdiferida
              from dual;
              if  NOT (instr(cur.NUMBERvalue,'|' || Vi_mes || '|',1) > 0) THEN
                vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cmbMesesDifInstalacion:'||cur.NUMBERvalue||';Vi_mes '||Vi_mes||';'||vn_resultado);exit;
              End If;
            End;
          when cur.controlname = 'cblTipoContratacion' then
            begin
              Vi_IsContractedInPI := yexpressionrules.fiiscontractinpi (vv_param(11));
                --verificar contratacion de planilla inicial
                if  Vi_IsContractedInPI = 1 and NOT (instr(cur.NUMBERvalue,'|' || 505964 || '|',1) > 0) THEN
                  vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoContratacion:Inicial:'||Vi_IsContractedInPI);exit;
                End if;
                --verificar contratacion de adicionales
                if  Vi_IsContractedInPI != 1 and NOT (instr(cur.NUMBERvalue,'|' || 505965 || '|',1) > 0) THEN
                  vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoContratacion:Adicional:'||Vi_IsContractedInPI);exit;
                End If;
                --verificar si el cliente ha sido migrado
                if Vi_IsContractedInPI != 1 and vv_param(21) = 1454 and NOT (instr(cur.NUMBERvalue,'|' || 505966 || '|',1) > 0) THEN
                  vn_resultado := 0;DBMS_OUTPUT.PUT_LINE ('Salio por cblTipoContratacion:Migrado:'||Vi_IsContractedInPI||':vv_param(21):'||vv_param(21));exit;
                End If;
           End;
          when cur.controlname = 'txtServicio' then
            Vn_Percent := to_NUMBER(substr(cur.NUMBERvalue,2,length(cur.NUMBERvalue)-2));DBMS_OUTPUT.PUT_LINE ('txtServicio:Vn_Percent:'||Vn_Percent);
          when cur.controlname = 'txtInstalacion' and nvl(vv_param(24),0) != 503952 then  -- If decoder type != HD
            Vn_Percent := to_NUMBER(substr(cur.NUMBERvalue,2,length(cur.NUMBERvalue)-2));DBMS_OUTPUT.PUT_LINE ('txtInstalacion:Vn_Percent:'||Vn_Percent);
          when cur.controlname = 'txtinstalacionHD' and nvl(vv_param(24),0) = 503952 then -- If decoder type = HD
            Vn_Percent := to_NUMBER(substr(cur.NUMBERvalue,2,length(cur.NUMBERvalue)-2));DBMS_OUTPUT.PUT_LINE ('txtinstalacionHD:Vn_Percent:'||Vn_Percent);
        Else
          vi_contador := 0;
        End Case;
      END LOOP;
    END IF;
    DBMS_OUTPUT.PUT_LINE ('Promocion=vn_resultado:'||vn_resultado);
    --Al final, si la variable vn_resultado es diferente de 0 significa que un cliente si tiene promocion
    IF vn_resultado > 0 THEN
      var1 := Pn_Amount;
      DBMS_OUTPUT.PUT_LINE ('Promocion_vn_resultado='||vn_resultado||':var1:'||var1);
      IF Vn_Percent = 100 THEN
        vn_precio := 0;
        DBMS_OUTPUT.PUT_LINE ('Promocion_Vn_Percent = 100='||Vn_Percent||':vn_precio:'||vn_precio);
      ELSE
        vn_precio := var1 - ((var1 * (Vn_Percent)) / 100);
        DBMS_OUTPUT.PUT_LINE ('Promocion_Vn_Percent!=100='||Vn_Percent||':vn_precio:'||vn_precio);
      END IF;
      IF vv_param(22) IS NOT NULL and vv_tipo = 'I' THEN
        -- Instalacion Diferida
        vn_precio := vn_precio / vn_instdiferida;
      END IF;
      IF vi_folder_id = 2 THEN
        DBMS_OUTPUT.PUT_LINE ('vi_folder_id:'||vi_folder_id||':vv_tipo:'||vv_tipo||'vv_param(19)'||vv_param(19));
        IF vv_param(19) = 1 AND vv_tipo = 'M' THEN
          var2 := 'Principal ' || var2;
          ELSIF vv_tipo = 'I' THEN
            var2 := 'Instalacion ' || var2;
            ELSIF vv_param(19) > 1 AND vv_tipo = 'M' THEN
              var2 := 'Adicional(es) ' || var2;
          END IF;
      ELSE
        IF vv_tipo = 'I' THEN
          var2 := 'Instalacion ' || var2;
        END IF;
      END IF;
      IF vn_precio < 0 or vn_precio is null THEN
        vn_precio := 0;
        DBMS_OUTPUT.PUT_LINE ('vn_precio < 0:' ||vn_precio);
      END IF;
      --------saca precio - porcentaje
      vn_respuesta := TO_CHAR (vn_precio || ';' || var2);
      --concatenar promo
      vn_respuesta:= vn_respuesta||';'||pn_promo_id;
      --
      vn_resultadofinal2 := vn_respuesta;
     /* IF (vn_resultadofinal2 IS NOT NULL) THEN
          YPKG_PROMOCIONES.GUARDAR_LOGS(vv_param(1),vv_param(4),vv_param(11),vv_param(12),pn_promo_id,pd_eventdate,vv_tipo);
          DBMS_OUTPUT.put_line ('GUARDAR_LOG:fv_Get_Promo:vn_resultadofinal2:'||vn_resultadofinal2);
      END IF;*/
    END IF;
    DBMS_OUTPUT.put_line ('FIN:fv_Get_Promo:vn_resultadofinal2:'||vn_resultadofinal2);
    RETURN vn_resultadofinal2;
  END fv_Get_Promo;
  FUNCTION fn_retornaprecio
    (
      vn_plan            NUMBER,
      vn_variante        NUMBER,
      vn_negocio         NUMBER,
      vn_producto        NUMBER,
      vn_costcenter      NUMBER,
      vv_isprincipal     NUMBER,
      pV_tipo            VARCHAR2,  -- 'M'/'I';id tipo decodificador
      vn_cpartytype      NUMBER,
      vn_accounttype     NUMBER,
      vn_countdecoders   NUMBER,
      vn_Contractitem_id NUMBER
     )
  RETURN NUMBER
  IS
    vn_preciot         NUMBER;
    vn_total           NUMBER;
    vn_citemmin        NUMBER;
    vn_nbprincipal     NUMBER;
    vv_AdditionalWhere VARCHAR2(1000);
    Vn_tipo            VARCHAR2(200);
    vn_tipo1           VARCHAR2(5);
  BEGIN
    vn_tipo:= pv_tipo;
    vn_tipo1:=substr(pV_tipo,1,1);
    DBMS_OUTPUT.PUT_LINE ('vn_tipo:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
    if instr(pV_tipo,';',1)> 0 and vn_tipo1 = 'I' then
      BEGIN
        select name
        into vn_tipo
        from trepvaluelistitems
        where id = substr(pV_tipo,instr(pV_tipo,';',1)+1,99);
        if (vn_tipo='Normal') then
          vn_tipo := substr(pV_tipo,1,1);
        end if;
      EXCEPTION WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE ('NO_DATA_FOUND:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
        vn_tipo := substr(pV_tipo,1,1);
      END;
      DBMS_OUTPUT.PUT_LINE ('IFvn_tipo:' ||vn_tipo);
    else
      vn_tipo := vn_tipo1;
      DBMS_OUTPUT.PUT_LINE ('Else:vn_tipo:' ||vn_tipo);
    end if;
    IF nvl(vn_countdecoders,0) = 0 THEN
      --Funcion que trae los precios del catalogo para clientes que no tienen countdecoders
      vv_AdditionalWhere := '  AND id =(select min(id)'||CHR(10)||
                            '           FROM ytcatalogoservicios lt1'||CHR(10)||
                            '           where lt1.tariffplan_id     = '||NVL(TO_CHAR(vn_plan),'NULL')||CHR(10)||
                            '           AND lt1.IsValid             = ''Y'' '||CHR(10)||
                            '           AND lt1.tariffplanvariant_id= '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '           AND lt1.business_id         = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '           AND lt1.product_id          = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '           AND lt1.costcenter_id       = '||NVL(TO_CHAR(vn_costcenter),'NULL')  ||CHR(10)||
                            '           AND lt1.cpartytype_id       = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '           AND lt1.accounttype_id      = '||NVL(TO_CHAR(vn_accounttype),'NULL')||CHR(10)||
                            '           )';
      --DBMS_OUTPUT.PUT_LINE ('vn_countdecoders='||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF vv_isprincipal = 1 AND vn_tipo = 'M' THEN
        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan',vv_AdditionalWhere);
        --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
          ELSIF vv_isprincipal = 0 AND vn_tipo = 'M' THEN
            vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
            --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
            ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND countdecoders is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
              if(vn_preciot is null) then
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot is null:' ||vn_preciot);
              end if;
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
              -----------------HD-----DVR-HD,DVR------------------------------------------
              ELSIF vn_tipo = 'HD' and vv_isprincipal in (1,0) THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot||':vv_isprincipal:'||vv_isprincipal);
                ELSIF vn_tipo in ('DVR HD','DVR') and vv_isprincipal in (1,0) THEN
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                  --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
      END IF;
    END IF;
    IF vn_countdecoders > 0 THEN
      vv_AdditionalWhere := 'AND id =(select min(id)'||CHR(10)||
                            '         FROM ytcatalogoservicios lt1'||CHR(10)||
                            '         where lt1.tariffplan_id     = '||NVL(TO_CHAR(vn_plan),'NULL')||CHR(10)||
                            '         AND lt1.IsValid             = ''Y'' '||CHR(10)||
                            '         AND lt1.tariffplanvariant_id= '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '         AND lt1.business_id         = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '         AND lt1.product_id          = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '         AND lt1.costcenter_id       = '||NVL(TO_CHAR(vn_costcenter),'NULL')  ||CHR(10)||
                            '         AND lt1.cpartytype_id       = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '         AND lt1.accounttype_id      = '||NVL(TO_CHAR(vn_accounttype),'NULL')||CHR(10)||
                            '         AND countdecoders is null)';
      --Funcion que trae los precios del catalogo para clientes que tengan countdecoders
      --DBMS_OUTPUT.PUT_LINE ('vn_countdecoders>'||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF (vv_isprincipal = 1 AND vn_tipo = 'M') OR (vn_nbprincipal = 1)  THEN
        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan',vv_AdditionalWhere);
        --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
          ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
            vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
            --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
            ELSIF (vv_isprincipal = 0 AND vn_tipo = 'M') OR (vn_nbprincipal = 0) THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',REPLACE(vv_AdditionalWhere,'AND countdecoders is null)',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL')||')');
              IF vn_preciot IS NULL THEN
                 vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
              END IF;
              --DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
              ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND countdecoders is null)',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL')||')');
                if(vn_preciot is null)then
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                  DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
                end if;
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot>0:' ||vn_preciot);
                ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
                  -------------------------HD-----DVR-HD,DVR------------------------------------------
                  ELSIF vn_tipo = 'HD' THEN
                    vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                    --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
                  ELSIF vn_tipo in ('DVR HD','DVR') THEN
                    vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                    --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
      END IF;
    END IF;
  RETURN vn_preciot;
  END fn_retornaprecio;
  FUNCTION fn_Field_Accounts
    (
      vn_account_id     NUMBER,
      vn_accounttype_id OUT NUMBER,
      vn_CpartyType_id  OUT NUMBER,
      vn_PaymentType    OUT NUMBER
    )
  RETURN NUMBER
  IS
    vn_ccenter NUMBER;
  BEGIN
    -- When function is used in Rating ( ctx.gi_ProcInstance_Id IS NOT NULL ), then use record chache
    -- because when recurring events ore usage records are processed they are sorted by Cparty_Id and in most cases this chache will be very effective.
    IF ctx.gi_ProcInstanceId IS NOT NULL
      AND NVL(gr_AccountFields.Account_Id,-1)     =  vn_account_id
      AND NVL(gr_AccountFields.ProcInstanceId,-1) =  ctx.gi_ProcInstanceId
    THEN
      --
      vn_ccenter        := gr_AccountFields.CCenter_Id;
      vn_accounttype_id := gr_AccountFields.Accounttype_id;
      vn_CpartyType_id  := gr_AccountFields.CpartyType_id;
      vn_PaymentType    := gr_AccountFields.PaymentType;
      RETURN vn_ccenter;
      --
    ELSE
      -- in other cases get data from database
      SELECT  costcenter_id, accounttype_id, dd.cpartytype_id, d.paymenttype_id
      INTO vn_ccenter, vn_accounttype_id, vn_CpartyType_id,vn_PaymentType
      FROM tamcpartyaccountd d, tamcontractingpartyd dd
      WHERE d.account_id = vn_account_id
      AND dd.cparty_id = d.cparty_id;
      -- and wtire to cache
      gr_AccountFields.ProcInstanceId  := ctx.gi_ProcInstanceId;
      gr_AccountFields.Account_Id      := vn_account_id;
      gr_AccountFields.CCenter_Id      := vn_ccenter;
      gr_AccountFields.Accounttype_id  := vn_accounttype_id;
      gr_AccountFields.CpartyType_id   := vn_CpartyType_id;
      gr_AccountFields.PaymentType     := vn_PaymentType;
      -- and returns value as before (before optimisation)
      RETURN vn_ccenter;
    END IF;
  END fn_Field_Accounts;
  FUNCTION YFN_GET_BELLOW_CITEM_ID(PN_CITEM_ID IN NUMBER, Pi_PRODUCT_ID IN INT)
  RETURN NUMBER
    IS
    VN_ROOT NUMBER;
  BEGIN
    SELECT CITEM_ID
    INTO VN_ROOT
    FROM TAMCONTRACTEDITEMD
    WHERE REQCITEM_ID = PN_CITEM_ID
    AND CITEM_ID IS NOT NULL
    AND PRODUCT_ID = Pi_PRODUCT_ID;
    RETURN VN_ROOT;
  exception when no_data_found then
    return PN_CITEM_ID;
  END YFN_GET_BELLOW_CITEM_ID;
   FUNCTION fn_retornaprecio1
    (
      vn_plan             NUMBER,
      vn_variante         NUMBER,
      vn_negocio          NUMBER,
      vn_producto         NUMBER,
      vn_costcenter       NUMBER,
      vv_isprincipal      NUMBER,
      PV_tipo             VARCHAR2,--M/I;TIPODECODIFICADOR
      vn_cpartytype       NUMBER,
      vn_accounttype      NUMBER,
      vn_countdecoders    NUMBER,
      vn_Contractitem_id  NUMBER,
      evenbegindate       DATE,
      parametros         VARCHAR2
    )
  RETURN NUMBER IS
    vn_preciot          NUMBER;
    vn_total            NUMBER;
    vn_citemmin         NUMBER;
    vn_nbprincipal      NUMBER;
    vv_AdditionalWhere  VARCHAR2(1000);
    vn_tipo             VARCHAR2(200);
    vn_tipo1            VARCHAR2(5);
    vn_tip_deco         NUMBER;
    pd_servicebegindate DATE;
    var                 VARCHAR2(200);
    NUM2                NUMBER:=0;
    pn_costcenter       NUMBER:=0;
    pn_sal              NUMBER:=0;
    vn_citem_id         number:=0;
  BEGIN
    pn_costcenter:=vn_costcenter;
    -----------Equipos HD-----------------------
    vn_tipo:= pv_tipo;
    vn_tipo1:=substr(pV_tipo,1,1);
    DBMS_OUTPUT.PUT_LINE ('vn_tipo:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
    IF instr(pV_tipo,';',1)> 0 and vn_tipo1 = 'I' THEN
      Begin
        select name
        into vn_tipo
        from trepvaluelistitems
        where id = substr(pV_tipo,instr(pV_tipo,';',1)+1,99);
        if (vn_tipo='Normal') then
          vn_tipo := substr(pV_tipo,1,1);
        end if;
      EXCEPTION WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE ('NO_DATA_FOUND:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
        vn_tipo := substr(pV_tipo,1,1);
      END;
      DBMS_OUTPUT.PUT_LINE ('IFvn_tipo:' ||vn_tipo);
    ELSE
      vn_tipo := vn_tipo1;
      DBMS_OUTPUT.PUT_LINE ('Else:vn_tipo:' ||vn_tipo);
    END IF;
    ---Nuevo Basico HD
    Begin
      Select CITEM_ID
      into vn_citem_id
      from TAMCONTRACTEDITEMD
      where id=vn_Contractitem_id;
      pn_sal:=YEXPRESSIONRULES.fncheckcitemaddsalinas(vn_citem_id);
    EXCEPTION WHEN NO_DATA_FOUND THEN
      DBMS_OUTPUT.PUT_LINE ('NO_DATA_FOUND:pn_sal' ||pn_sal);
      pn_sal:=0;
    END;
    DBMS_OUTPUT.PUT_LINE ('pn_sal:' ||pn_sal);
    if (vn_producto in (153,10,25,135,136,173,174) and pn_sal=1) then
      if(vn_costcenter in (8,3) and vn_plan=1)then
        pn_costcenter:=3;
      else
        pn_costcenter:=11;
      end if;
      DBMS_OUTPUT.PUT_LINE ('Adicional_Salinas:'||vn_Contractitem_id);
    else
      pn_costcenter:=vn_costcenter;
      DBMS_OUTPUT.PUT_LINE ('No_Adicional_Salinas:'||vn_Contractitem_id||':pn_costcenter:'||pn_costcenter);
    end if;
    -----ServiceBegin Date------------------
    pd_servicebegindate:=evenbegindate;
    var:=''||Trunc(pd_servicebegindate)||'';
    var:=REPLACE(var,' 00:00:00','');
    DBMS_OUTPUT.PUT_LINE ('var:' ||REPLACE(var,' 00:00:00',''));
    IF nvl(vn_countdecoders,0) = 0 THEN
      vv_AdditionalWhere := '  AND IsValid              = ''Y'' '||CHR(10)||
                            '  AND tariffplanvariant_id = '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '  AND business_id          = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '  AND product_id           = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '  AND costcenter_id        = '||NVL(TO_CHAR(pn_costcenter),'NULL')  ||CHR(10)||
                            '  AND cpartytype_id        = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '  AND accounttype_id       = '||NVL(TO_CHAR(vn_accounttype),'NULL') ||CHR(10)||
                            '  AND '''||var||''' between dfrom AND nvl(dto,'''||var||''')';
      --Funcion que trae los precios del catalogo para clientes que no tienen countdecoders
      DBMS_OUTPUT.PUT_LINE ('vn_countdecoders='||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF vv_isprincipal = 1 AND vn_tipo = 'M' THEN
         vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan',vv_AdditionalWhere);
         DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
          ELSIF vv_isprincipal = 0 AND vn_tipo = 'M' THEN
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
            ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND countdecoders is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
              if(vn_preciot is null)then
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
              end if;
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot>0:' ||vn_preciot);
              -----------------HD-----DVR-HD,DVR------------------------------------------
              ELSIF vn_tipo = 'HD' and vv_isprincipal in (1,0) THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
              ELSIF vn_tipo in ('DVR HD','DVR')  and vv_isprincipal in (1,0) THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
        end if;
      END IF;
    IF vn_countdecoders > 0 THEN
      vv_AdditionalWhere := '  AND IsValid              = ''Y'' '||CHR(10)||
                            '  AND tariffplanvariant_id = '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '  AND business_id          = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '  AND product_id           = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '  AND costcenter_id        = '||NVL(TO_CHAR(pn_costcenter),'NULL')  ||CHR(10)||
                            '  AND cpartytype_id        = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '  AND accounttype_id       = '||NVL(TO_CHAR(vn_accounttype),'NULL') ||CHR(10)||
                            '  AND '''||var||''' between dfrom AND nvl(dto,'''||var||''')'||CHR(10)||
                            '  AND COUNTDECODERS is null';
      --Funcion que trae los precios del catalogo para clientes que tengan countdecoders
      DBMS_OUTPUT.PUT_LINE ('vn_countdecoders>'||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF (vv_isprincipal = 1 AND vn_tipo = 'M') OR (vn_nbprincipal = 1)  THEN
        vn_preciot :=NULL;
        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
        IF vn_preciot IS NULL THEN
           vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan',vv_AdditionalWhere);
           DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
        END IF;
        DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
          ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
            vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
            DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
            ELSIF (vv_isprincipal = 0 AND vn_tipo = 'M') OR (vn_nbprincipal = 0) THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
              IF vn_preciot IS NULL THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
              END IF;
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
              ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
                vn_preciot :=NULL;
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                if(vn_preciot is null)then
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                  DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
                end if;
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot>0:' ||vn_preciot);
                ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
                  -----------------HD-----DVR-HD,DVR----------------------------------------------------------------------------------------------
                  ELSIF vn_tipo = 'HD' THEN
                    vn_preciot :=NULL;
                    vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                    if(vn_preciot is null)then
                      vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                      DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
                    end if;
                    --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
                    ELSIF vn_tipo in ('DVR HD','DVR') THEN
                      vn_preciot :=NULL;
                      vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                      if(vn_preciot is null)then
                        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                        DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
                      end if;
                      --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
      END IF;
    END IF;
    if(vn_preciot is null)then
      vn_preciot:=0;
    end if;
  RETURN vn_preciot;
  END fn_retornaprecio1;
 FUNCTION fn_Tipo_Deco(
    pn_producto  NUMBER,
    pn_citem_id NUMBER
  )
  RETURN NUMBER
  is
    VV VARCHAR2(100);
    vn_product_id NUMBER;
    vn_citem_id NUMBER;
    vn_tip_deco NUMBER;
  BEGIN
    select nvl(y_tvc_production_am.fv_devuelvepropiedad('Tipo Decodificador',t4.product_id,t4.citem_id),0)
    into vn_tip_deco
    FROM tamcontracteditemd t4
    WHERE t4.citem_id= ( Select REQCITEM_ID
                         From tamcontracteditemd
                         where citem_id=pn_citem_id
                        --and state='A'
                         and product_id=pn_producto
                         --and dto is null
                         and isvalid='Y'
                       )
    --AND t4.state ='A'
    --AND t4.dto IS NULL
    AND t4.isvalid ='Y';
    return vn_tip_deco;
  END;
  FUNCTION fn_buscaprincipalNBHD(
    vn_cparty_id  NUMBER,
        vn_account_id NUMBER,
    vn_id_citem NUMBER,
    pn_citem_id NUMBER)
  RETURN NUMBER
  IS
    vn_citemprincipal NUMBER;
    vn_citemcompar NUMBER;
    vn_padre NUMBER;
    vn_padre_id NUMBER;
  BEGIN
    ------------Obtener el Padre del Nuevo_Basico_HD
    select t4.id,t4.citem_id
    into vn_padre_id,vn_padre
    FROM tamcontracteditemd t4
    WHERE t4.citem_id= (  Select REQCITEM_ID
                          From tamcontracteditemd
                          Where id=vn_id_citem
                          and cparty_id =vn_cparty_id
                          AND cpartyaccount_id = vn_account_id
                          and state='A'
                          and dto is null
                          and isvalid='Y'
                        )
    AND t4.state ='A'
    AND t4.dto IS NULL
    AND t4.isvalid ='Y';
    DBMS_OUTPUT.put_line ('Deco Padre NBHD_Citem_id:' || vn_padre||':vn_padre_id:'||vn_padre_id);
    ---------------obtencion del primer deco HD
    Select MIN(t3.citem_id)
    into vn_citemprincipal
    FROM tamcontracteditemd t3
    WHERE  t3.cparty_id =vn_cparty_id
    AND t3.cpartyaccount_id = vn_account_id
    AND t3.state ='A'
    AND t3.dto IS NULL
    AND t3.isvalid ='Y'
    AND (NVL(y_tvc_production_am.fv_devuelvepropiedad('Tipo Decodificador',t3.product_id, t3.citem_id),0))= (SELECT ID FROM trepvaluelistitems
    WHERE NAME = 'HD');
    DBMS_OUTPUT.put_line ('deco principal_HD:' || vn_citemprincipal);
    -----------DecoHD
    SELECT citem_id
    INTO vn_citemcompar
    FROM tamcontracteditemd
    WHERE ID = vn_padre_id;
    DBMS_OUTPUT.put_line ('deco secundario_HD:' || vn_citemcompar);
    IF (vn_citemprincipal) = (vn_citemcompar) then
      DBMS_OUTPUT.put_line ('recibe promo:' || vn_citemcompar);
      return 1;
    ELSE
      DBMS_OUTPUT.put_line ('no recibe promo:' || vn_citemcompar);
      return 0;
    end if;
  EXCEPTION WHEN others THEN
    return 0;
    dbms_output.put_line('no da promo');
  END fn_buscaprincipalNBHD;
  PROCEDURE GUARDAR_LOGS (pn_cparty_id NUMBER,pn_account_id NUMBER,pn_citem_id NUMBER,pn_prodcut_id NUMBER,pn_promocion NUMBER,pd_servcio DATE,tipo VARCHAR2)
  IS
    EXISTE NUMBER;
    descripcion VARCHAR2(900);
  BEGIN
    select description
    into descripcion
    from y_catalogpromotions
    where id=pn_promocion
    and isvalid='Y';
    SELECT COUNT(*)
    INTO EXISTE
    FROM ytbl_promocioneslog_mod
    WHERE cparty_id=pn_cparty_id
    AND account_id=pn_account_id
    AND citem_id=pn_citem_id
    AND promo_id=pn_promocion;
    IF (EXISTE=0) THEN
      INSERT INTO ytbl_promocioneslog_mod(cparty_id,account_id,citem_id,product_id,promo_id,Creation_Date,EventBeginDate,comentarios)
      VALUES(pn_cparty_id,pn_account_id,pn_citem_id,pn_prodcut_id,pn_promocion,SYSDATE,pd_servcio,tipo||':'||descripcion);
    ELSE
      UPDATE ytbl_promocioneslog_mod SET Creation_Date=SYSDATE,EventBeginDate=pd_servcio,comentarios=tipo||':'||descripcion
      WHERE cparty_id=pn_cparty_id
      AND account_id=pn_account_id
      AND citem_id=citem_id
      AND promo_id=pn_promocion;
    END IF;
  END GUARDAR_LOGS;
  FUNCTION YFN_CITEMCESDESR(PN_CITEM_ID IN NUMBER) RETURN NUMBER
IS
	VN_CESDER		NUMBER;
BEGIN
    SELECT COUNT(*)
    INTO VN_CESDER
    FROM TAMCONTRACTEDITEMD T2
    WHERE T2.ID = PN_CITEM_ID
    AND EXISTS (SELECT 1 FROM TWFLPROCESSINSTANCES T1
                 WHERE T1.ID = T2.CONTRACT_ID
                 AND T1.PROCESSDEF_ID = 22
                 );
	RETURN VN_CESDER;
END YFN_CITEMCESDESR;
FUNCTION FN_RETORNAPRECIO1EMP
    (
      vn_plan             NUMBER,
      vn_variante         NUMBER,
      vn_negocio          NUMBER,
      vn_producto         NUMBER,
      vn_costcenter       NUMBER,
      vv_isprincipal      NUMBER,
      PV_tipo             VARCHAR2,--M/I;TIPODECODIFICADOR
      vn_cpartytype       NUMBER,
      vn_accounttype      NUMBER,
      vn_countdecoders    NUMBER,
      vn_Contractitem_id  NUMBER,
      evenbegindate       DATE,
      parametros         VARCHAR2
    )
  RETURN NUMBER IS
    vn_preciot          NUMBER;
    vn_total            NUMBER;
    vn_citemmin         NUMBER;
    vn_nbprincipal      NUMBER;
    vv_AdditionalWhere  VARCHAR2(1000);
    vn_tipo             VARCHAR2(200);
    vn_tipo1            VARCHAR2(5);
    vn_tip_deco         NUMBER;
    pd_servicebegindate DATE;
    var                 VARCHAR2(200);
    NUM2                NUMBER:=0;
    pn_costcenter       NUMBER:=0;
    pn_sal              NUMBER:=0;
    vn_citem_id         number:=0;
  BEGIN
    pn_costcenter:=vn_costcenter;
    -----------Equipos HD-----------------------
    vn_tipo:= pv_tipo;
    vn_tipo1:=substr(pV_tipo,1,1);
    DBMS_OUTPUT.PUT_LINE ('vn_tipo:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
    IF instr(pV_tipo,';',1)> 0 and vn_tipo1 = 'I' THEN
      Begin
        select name
        into vn_tipo
        from trepvaluelistitems
        where id = substr(pV_tipo,instr(pV_tipo,';',1)+1,99);
        if (vn_tipo='Normal') then
          vn_tipo := substr(pV_tipo,1,1);
        end if;
      EXCEPTION WHEN NO_DATA_FOUND THEN
        DBMS_OUTPUT.PUT_LINE ('NO_DATA_FOUND:' ||pv_tipo||':vn_tipo1:'||vn_tipo1);
        vn_tipo := substr(pV_tipo,1,1);
      END;
      DBMS_OUTPUT.PUT_LINE ('IFvn_tipo:' ||vn_tipo);
    ELSE
      vn_tipo := vn_tipo1;
      DBMS_OUTPUT.PUT_LINE ('Else:vn_tipo:' ||vn_tipo);
    END IF;
    ---Nuevo Basico HD
    Begin
      Select CITEM_ID
      into vn_citem_id
      from TAMCONTRACTEDITEMD
      where id=vn_Contractitem_id;
      pn_sal:=YEXPRESSIONRULES.fncheckcitemaddsalinas(vn_citem_id);
    EXCEPTION WHEN NO_DATA_FOUND THEN
      DBMS_OUTPUT.PUT_LINE ('NO_DATA_FOUND:pn_sal' ||pn_sal);
      pn_sal:=0;
    END;
    DBMS_OUTPUT.PUT_LINE ('pn_sal:' ||pn_sal);
    if (vn_producto in (153,10,25,135,136) and pn_sal=1) then
      if(vn_costcenter in (8,3) and vn_plan=1)then
        pn_costcenter:=3;
      else
        pn_costcenter:=11;
      end if;
      DBMS_OUTPUT.PUT_LINE ('Adicional_Salinas:'||vn_Contractitem_id);
    else
      pn_costcenter:=vn_costcenter;
      DBMS_OUTPUT.PUT_LINE ('No_Adicional_Salinas:'||vn_Contractitem_id||':pn_costcenter:'||pn_costcenter);
    end if;
    -----ServiceBegin Date------------------
    pd_servicebegindate:=evenbegindate;
    var:=''||Trunc(pd_servicebegindate)||'';
    var:=REPLACE(var,' 00:00:00','');
    DBMS_OUTPUT.PUT_LINE ('var:' ||REPLACE(var,' 00:00:00',''));
    IF nvl(vn_countdecoders,0) = 0 THEN
      vv_AdditionalWhere := '  AND IsValid              = ''Y'' '||CHR(10)||
                            '  AND tariffplanvariant_id = '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '  AND business_id          = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '  AND product_id           = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '  AND costcenter_id        = '||NVL(TO_CHAR(pn_costcenter),'NULL')  ||CHR(10)||
                            '  AND cpartytype_id        = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '  AND accounttype_id       = '||NVL(TO_CHAR(vn_accounttype),'NULL') ||CHR(10)||
                            '  AND '''||var||''' between dfrom AND nvl(dto,'''||var||''')';
      --Funcion que trae los precios del catalogo para clientes que no tienen countdecoders
      DBMS_OUTPUT.PUT_LINE ('vn_countdecoders='||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF vv_isprincipal = 1 AND vn_tipo = 'M' THEN
         vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','PRECIO_GUIA',vv_AdditionalWhere);
         DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
          ELSIF vv_isprincipal = 0 AND vn_tipo = 'M' THEN
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:'||vn_preciot);
            ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND countdecoders is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
              if(vn_preciot is null)then
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
              end if;
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot>0:' ||vn_preciot);
              -----------------HD-----DVR-HD,DVR------------------------------------------
              ELSIF vn_tipo = 'HD' and vv_isprincipal in (1,0) THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
              ELSIF vn_tipo in ('DVR HD','DVR')  and vv_isprincipal in (1,0) THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
        end if;
      END IF;
    IF vn_countdecoders > 0 THEN
      vv_AdditionalWhere := '  AND IsValid              = ''Y'' '||CHR(10)||
                            '  AND tariffplanvariant_id = '||NVL(TO_CHAR(vn_variante),'NULL')    ||CHR(10)||
                            '  AND business_id          = '||NVL(TO_CHAR(vn_negocio),'NULL')     ||CHR(10)||
                            '  AND product_id           = '||NVL(TO_CHAR(vn_producto),'NULL')    ||CHR(10)||
                            '  AND costcenter_id        = '||NVL(TO_CHAR(pn_costcenter),'NULL')  ||CHR(10)||
                            '  AND cpartytype_id        = '||NVL(TO_CHAR(vn_cpartytype),'NULL')  ||CHR(10)||
                            '  AND accounttype_id       = '||NVL(TO_CHAR(vn_accounttype),'NULL') ||CHR(10)||
                            '  AND '''||var||''' between dfrom AND nvl(dto,'''||var||''')'||CHR(10)||
                            '  AND COUNTDECODERS is null';
      --Funcion que trae los precios del catalogo para clientes que tengan countdecoders
      DBMS_OUTPUT.PUT_LINE ('vn_countdecoders>'||vn_countdecoders||':vv_AdditionalWhere:' ||vv_AdditionalWhere);
      IF (vv_isprincipal = 1 AND vn_tipo = 'M') OR (vn_nbprincipal = 1)  THEN
        vn_preciot :=NULL;
        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','PRECIO_GUIA',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
        IF vn_preciot IS NULL THEN
           vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','PRECIO_GUIA',vv_AdditionalWhere);
           DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
        END IF;
        DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
        ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
          vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','precio_plan_conx',vv_AdditionalWhere);
          DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
          ELSIF vv_isprincipal = 1 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
            vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
            DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
            ELSIF (vv_isprincipal = 0 AND vn_tipo = 'M') OR (vn_nbprincipal = 0) THEN
              vn_preciot :=NULL;
              vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
              IF vn_preciot IS NULL THEN
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','mensualidad_adicional',vv_AdditionalWhere);
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
              END IF;
              DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_preciot:' ||vn_preciot);
              ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders < 5 THEN
                vn_preciot :=NULL;
                vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                if(vn_preciot is null)then
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adicional',vv_AdditionalWhere);
                  DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot is null:' ||vn_preciot);
                end if;
                DBMS_OUTPUT.PUT_LINE ('vv_isprincipal:'||vv_isprincipal||':vn_tipo:'||vn_tipo||':vn_preciot>0:' ||vn_preciot);
                ELSIF vv_isprincipal = 0 AND vn_tipo = 'I' AND vn_countdecoders >= 5 THEN
                  vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','conexion_adi_solicitado',vv_AdditionalWhere);
                  -----------------HD-----DVR-HD,DVR----------------------------------------------------------------------------------------------
                  ELSIF vn_tipo = 'HD' THEN
                    vn_preciot :=NULL;
                    vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                    if(vn_preciot is null)then
                      vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION3',vv_AdditionalWhere);
                      DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
                    end if;
                    --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
                    ELSIF vn_tipo in ('DVR HD','DVR') THEN
                      vn_preciot :=NULL;
                      vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',REPLACE(vv_AdditionalWhere,'AND COUNTDECODERS is null',NULL)||' AND countdecoders = '||NVL(TO_CHAR(vn_countdecoders),'NULL'));
                      if(vn_preciot is null)then
                        vn_preciot := fvGetLookupValue('ytcatalogoservicios',vn_plan,'tariffplan_id','INSTALACION4',vv_AdditionalWhere);
                        DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||'vn_preciot is null:' ||vn_preciot);
                      end if;
                      --DBMS_OUTPUT.PUT_LINE ('vn_tipo:'||vn_tipo||':vn_tipo1:'||vn_tipo1||':vn_preciot:'||vn_preciot);
      END IF;
    END IF;
    if(vn_preciot is null)then
      vn_preciot:=0;
    end if;
  RETURN vn_preciot;
  END FN_RETORNAPRECIO1EMP;
END;
