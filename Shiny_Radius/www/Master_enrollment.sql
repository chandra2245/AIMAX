/*************************************************************************************************************/
/**Master enrollment table is to list all the HC patients with their respective HC services 
and the corresponding minimum enrollment dates. ***********************************/
/******For CRM filter only for Retention Campaign*************/
/******For Ambassador, Filter for OPT TYPE='AMB' and OPT VAL CD='I'*************************/
/******For Ambassador OPT OUT, Filter for OPT TYPE='AMB' and OPT VAL CD='O'*************************/
/***Appending datasets of ambassador and Epsilon feed sources for diffrent services corresponding 
minimum enrollment dates**/
/****Getting the minimum enrollment dates from Fulfillment, activity_refined, enrollment, patient app, crm tables.**/
/*************************************************************************************************************/

drop table hc_data_integration_cws.Master_Enrollment_Temp_1;

create table hc_data_integration_cws.Master_Enrollment_Temp_1 as 
select 
    abbvie_indiv_id, 
    Resource, 
    SubResource, 
    promo_rsp_id, 
    CASE 
        WHEN g.channel IS NULL then 'UNKNOWN'
        WHEN UPPER(channel) = "OTHERS" THEN "OTHER"
        ELSE UPPER(channel) 
    end as channel,
    min(Enrollment_date) as Enrollment_date
from
(
    select abbvie_indiv_id,  Resource,  SubResource, promo_rsp_id, channel,  MIN(Enrollment_date) as Enrollment_date
    from hc_data_integration_cws.hc_ambassador_activity_refined
	GROUP BY abbvie_indiv_id,  Resource,  SubResource, promo_rsp_id, channel
	
Union all
    select a.abbvie_indiv_id, Resource, 
    SubResource, 
    a.promo_rsp_id, 
    case
        when Resource = 'RELATIONSHIP MARKETING' then 'TEXT' 
        else d.channel
    end as channel, 
    min(a.rsp_dt) as Enrollment_date
    from 
        (
        select 
        abbvie_indiv_id, Resource, SubResource, promo_rsp_id, rsp_dt,
            RANK() over 
            (PARTITION BY abbvie_indiv_id, SubResource, promo_rsp_id order by rsp_dt ASC) as RowNo 
        from hc_data_integration_cws.hc_service_enrl_refined
        ) A  
        LEFT JOIN epsilon_psp_hum_mktg.eps_response_hum_vw C
             on a.promo_rsp_id = c.promo_rsp_id 
        LEFT JOIN hc_data_integration_cws.hc_channel_mapping D 
            on upper(c.vehicle_dsc) = d.vehicle_dsc AND upper(c.channel_dsc)=d.channel_dsc
        where RowNo = 1
        group by a.abbvie_indiv_id, Resource, SubResource, a.promo_rsp_id, d.channel
 
Union all

    select a.abbvie_indiv_id, Resource, 
    case 
        when SubResource = 'SHARPS_AMB' then 'SHARPS'
        when SubResource = 'INJECTION TRAINING KIT_AMB' then 'INJECTION TRAINING KIT'
        else SubResource 
    end as SubResource,
    a.promo_rsp_id, 
    case 
        when SubResource IN ('SHARPS_AMB', 'INJECTION TRAINING KIT_AMB') then 'AMBASSADOR'
        else d.channel 
    end as channel, 
    min(interaction_date) as Enrollment_date
    from 
        (
        select 
        abbvie_indiv_id, Resource, SubResource, promo_rsp_id, interaction_date, 
            RANK() over 
            (PARTITION BY abbvie_indiv_id, SubResource, promo_rsp_id order by interaction_date ASC) as RowNo 
        from hc_data_integration_cws.hc_service_ffl_refined
        ) A LEFT JOIN  
        epsilon_psp_hum_mktg.eps_response_hum_vw C
             on a.promo_rsp_id = c.promo_rsp_id 
             LEFT JOIN hc_data_integration_cws.hc_channel_mapping D 
                on upper(c.vehicle_dsc) = d.vehicle_dsc AND upper(c.channel_dsc)=d.channel_dsc
        where RowNo = 1
        group by a.abbvie_indiv_id, Resource, SubResource, a.promo_rsp_id, channel

Union all
    --App
   select abbvie_indiv_id, 'PATIENT APP' as Resource, 'PATIENT APP' as SubResource, 'APP99999' as promo_rsp_id, 'APP' as channel, min(rsp_dt) as Enrollment_date
    from epsilon_psp_hum_mktg.eps_papp_hum_vw
    group by abbvie_indiv_id
  

Union all
    --CRM
    
    select f.abbvie_indiv_id,'RELATIONSHIP MARKETING' as Resource, 
    upper(channel_dsc) as SubResource,'CRM99999' as promo_rsp_id, 'CRM99999' as channel, min(extract_date) as Enrollment_date
    from (select a.abbvie_indiv_id, channel_dsc, extract_date
    from epsilon_psp_hum_mktg.eps_crm_hum_vw a
    INNER JOIN epsilon_psp_hum_mktg.eps_base_indiv_hum_vw b
    on a.abbvie_indiv_id = b.abbvie_indiv_id
    where marketing_objective = 'Retention'
    and 
    KIT_NM NOT IN ('HUMMEDRMDR_EM3','MYH_MEDREM_EM2','MYH_MEDREM_EM_C','MYH_MEDREM_EM','HC_MEDREM_EM_A','HUMMEDRMDR_EM2')
    and kit_cd not like 'SHRPS_KIT%' and kit_CD not like 'HUMMEDRMDR_EM%'
    ) F 
    GROUP BY f.abbvie_indiv_id, UPPER(channel_dsc)

    --channel_dsc in ('Email','Direct Mail')        
    
    --select f.abbvie_indiv_id,'RELATIONSHIP MARKETING' as Resource, 
    --upper(channel_dsc) as SubResource,'CRM99999' as promo_rsp_id, 'CRM99999' as channel, min(extract_date) as Enrollment_date
    --from (select a.abbvie_indiv_id, channel_dsc, extract_date, RANK() over 
    --    (PARTITION BY a.abbvie_indiv_id order by extract_date asc) AS RowNo  
    --    from epsilon_psp_hum_mktg.eps_crm_hum_vw a
    --    INNER JOIN epsilon_psp_hum_mktg.eps_base_indiv_hum_vw b
    --         on a.abbvie_indiv_id = b.abbvie_indiv_id
    --    where channel_dsc in ('Email','Direct Mail')  and  marketing_objective = 'Retention'
    --    and kit_cd not IN ('HUMMEDRMDR_EM3','MYH_MEDREM_EM_C', 'HC_MEDREM_EM_A', 'HC_MEDREM_EM_B', 'HUM_SMS_RMND_A', 'HUM_SMS_DBL_A', 
    --    'HUM_IVR_RMND_A', 'MYH_MEDREM_EM2', 'MYHUM_HUGO_KIT', 'HUMCOM_HUGO_FT', 'SHRPS_KIT_A', 'SHRPS_KIT_B', 
    --    'SHRPS_KIT_C', 'SHRPS_KIT_D', 'SHRPS_KIT_E', 'SHRPS_KIT_F', 'SHRPS_KIT_G', 'SHRPS_KIT_H', 'MYHUM_ITK_KIT', 
    --    'HUMCOM_ITK_FT', 'PAPP_RMNDR','HUMMEDRMDR_EM')
    --    ) F WHERE F.RowNo = 1 
    --GROUP BY f.abbvie_indiv_id, UPPER(channel_dsc)

union all
--SHARPS COLLATERAL
select B.abbvie_indiv_id,'SHARPS' as Resource, 'SHARPS' as SubResource, 
    'SHR99999' as promo_rsp_id, 'AMBASSADOR' as channel, 
    min(from_timestamp(a.`date`,'yyyy-MM-dd') )  as Enrollment_date 
    from
    (select *, case 
             when cast(provided as tinyint)=1 then 'provided'
             when cast(ordered as tinyint)=1 then 'ordered'
             end as outcome
    from quintiles_psp_immun_mktg.ambassador_interaction_veeva_sharps_collateral_vw where upper(product) like '%SHARP%' and (cast(ordered as tinyint)<>0 or cast(provided as tinyint)<>0)) a 
    inner join 
    epsilon_psp_hum_mktg.eps_base_indiv_hum_vw B  on A.abbvie_patient_id = B.abbvie_patient_id where B.abbvie_indiv_id is not null
    group by B.abbvie_indiv_id
    
union all

--ITK COLLATERAL
    select B.abbvie_indiv_id,'INJECTION TRAINING KIT' as Resource, 'INJECTION TRAINING KIT' as SubResource, 
    'ITK99999' as promo_rsp_id, 'AMBASSADOR' as channel, 
    min(from_timestamp(a.`date`,'yyyy-MM-dd'))  as Enrollment_date 
    from
    (select *, case 
             when cast(provided as tinyint) =1 then 'provided'
             when cast(ordered as tinyint) =1 then 'ordered'
             end as outcome
    from quintiles_psp_immun_mktg.ambassador_interaction_veeva_sharps_collateral_vw where 
    (upper(product) LIKE '%HC INJECTION TRAINING MECHANIC%' or upper(product) like '%TALKING PEN INJECTION KIT%' or 
    upper(product) like '%HC INJECTION TRAINING KIT%')
     and (cast(ordered as tinyint)<>0 or cast(provided as tinyint)<>0)) a 
    inner join 
    epsilon_psp_hum_mktg.eps_base_indiv_hum_vw B  on A.abbvie_patient_id = B.abbvie_patient_id where B.abbvie_indiv_id is not null
    group by B.abbvie_indiv_id


    union all
    
    --QUINTILES FEED
    
    
    select distinct x.abbvie_indiv_id, resource, subresource, promo_rsp_id, channel, enrollment_date from (
    select a.*,
    CASE 
    						WHEN DISPOSITION IS NULL 
    						THEN 
    								CASE 
    								WHEN upper(master_activity_type)  in ('SPECIALIST INITIATED', 'AMBASSADOR INITIATED', 'APPROVED FOLLOW-UP NOTE SENT') 
    								OR upper(activity_sub_type)  in ('LEFT MESSAGE', 'UNABLE TO LEAVE MESSAGE', 'TRANSFER BACK', 'NRE HCP OUTREACH', 'PATIENT DETAILED MESSAGE', 'MILESTONE', 'SENTIMENT', 'THANK YOU') 
    								THEN 0
    								ELSE 1
    								END 
    						ELSE 
    								CASE 
    								WHEN UPPER(DISPOSITION) in ('ENGAGED') 
    								THEN 1
    								ELSE 0
    								END 
    				END AS ACTIVITY_STATUS 
    				from (
    select 
        abbvie_indiv_id,  
        'NURSE INJECTION TRAINING' as Resource, 
        'NURSE INJECTION TRAINING' AS SubResource, 
        'NIT99999' as promo_rsp_id,
        case 
            when rtrim(a.account_record_type) = c.account_record_type and a.Enrollment_Channel_name = c.Enrollment_Channel and a.Enrollment_Type = c.Enrollment_Type then c.final_channel
            else 'UNKNOWN'
        end as Channel,
        master_activity_type,
        activity_sub_type,
        --Coalesce(from_unixtime(unix_timestamp(patient_triage_date ,'MM/dd/yyyy'), 'yyyy-MM-dd'),date_distributed) as Enrollment_date,
        DIsposition,
        DELIVERY_CHANNEL,
        a.activity_date as enrollment_date
    from abv_psp_amb_interaction_hum_mktg.hc_ambassador_interaction_master_tbl a
    left join hc_data_integration_cws.channel_mapping_ambassador c
    on a.account_record_type = c.account_record_type and a.Enrollment_Channel_name = c.Enrollment_Channel and
    a.Enrollment_Type = c.Enrollment_Type
    where abbvie_indiv_id is NOT NULL  
    ORDER BY abbvie_indiv_id) a)x
    WHERE (UPPER(DELIVERY_CHANNEL) LIKE('%HHRN%') OR UPPER(master_activity_type) LIKE('%HHRN%') ) AND ACTIVITY_STATUS=1 
--    and upper(DIsposition) = 'ENGAGED'
    --group by x.abbvie_indiv_id, resource, subresource, promo_rsp_id, channel
    
    ) g
    where abbvie_indiv_id is not null
group by abbvie_indiv_id, Resource, SubResource, promo_rsp_id, channel
ORDER BY abbvie_indiv_id;
Select count(*) from hc_data_integration_cws.master_enrollment_temp_1
/*6456466*/
/*6457445*/
/*****************************************************************************************************************************/
/******Get Minimum Enrollment date from the above table at Indiv Id and Service level to get the Master enrollment table******/
/*****************************************************************************************************************************/
Drop table hc_data_integration_cws.Master_Enrollment_Temp_bckp;
Create table  hc_data_integration_cws.Master_Enrollment_Temp_bckp as  
Select * from hc_data_integration_cws.Master_Enrollment_Temp;
/*10987543*/
drop table hc_data_integration_cws.Master_Enrollment_Temp;

create table hc_data_integration_cws.Master_Enrollment_Temp as 
select g.abbvie_indiv_id, Resource, SubResource,
    case when g.promo_rsp_id ='CRM99999' then h.promo_rsp_id else g.promo_rsp_id end as promo_rsp_id, CASE WHEN g.channel IS NULL  
    then 'UNKNOWN' when g.channel ='CRM99999' then h.channel else g.channel end as channel, enrollment_date
    from hc_data_integration_cws.Master_Enrollment_Temp_1 g
    left join 
    (
    select abbvie_indiv_id, promo_rsp_id, channel
    from (
    select abbvie_indiv_id, promo_rsp_id, channel,
    RANK() over (PARTITION BY abbvie_indiv_id order by Enrollment_date ASC) as RowNo 
    from hc_data_integration_cws.Master_Enrollment_Temp_1
    where channel NOT IN ('NULL', 'CRM99999')
    ) A
    WHERE A.RowNo = 1
    ) h on g.abbvie_indiv_id = h.abbvie_indiv_id;
    /*10951261*/

/*****************************************************************************************************************************/
/******Excluding the records having subresource('MED REM APP', 'AMBASSADOR DISTRIBUTED MATERIALS')******/
/*****************************************************************************************************************************/

drop table hc_data_integration_cws.Master_Enrollment_temp2;
    
    create table hc_data_integration_cws.Master_Enrollment_temp2 as
    select
        abbvie_indiv_id, 
        case 
            when upper(x.Resource) in ('','NULL') or upper(x.Resource) is NULL then 'UNKNOWN'
            ELSE x.Resource
        end as Resource,
        case 
            when upper(x.SubResource) in ('','NULL') or upper(x.SubResource) is NULL then 'UNKNOWN'
            ELSE x.SubResource
        end as SubResource,
        case 
            when upper(x.channel) in ('','NULL') or upper(x.channel) is NULL then 'UNKNOWN'
            ELSE x.channel
        end as channel,
        Enrollment_date
    from 
    (
        select abbvie_indiv_id, Resource, SubResource, channel, Enrollment_date, RANK() over 
            (PARTITION BY abbvie_indiv_id, Resource, SubResource, channel order by Enrollment_date asc) AS RowNo  
        from 
        (select abbvie_indiv_id, Resource, SubResource, channel, min(Enrollment_date) as Enrollment_date
        from hc_data_integration_cws.Master_Enrollment_temp 
        where SubResource NOT IN ('MED REM APP', 'AMBASSADOR DISTRIBUTED MATERIALS')
        group by abbvie_indiv_id, Resource, SubResource, channel) Y
    ) X
    where X.RowNo = 1
    order by abbvie_indiv_id;
    /*4859177*/
    
    /*4869404 old count*/

-- removing CRM records if hc_opt is not 'I' and enrolled for only one resource

drop table hc_data_integration_cws.master_enrollment_CRM;

create table  hc_data_integration_cws.master_enrollment_CRM as 
select x.abbvie_indiv_id,x.resource,x.subresource,x.channel,x.enrollment_date from(
    select c.*,d.Res_ct,
      case when resource='RELATIONSHIP MARKETING' and hc_opt_status is NULL and res_ct=1 then 0
      else 1 end as flag from
        (select a.*,b.hc_opt_status from  hc_data_integration_cws.master_enrollment_temp2 a left join 
          (select distinct abbvie_indiv_id, AMB_opt_status AS hc_opt_status from epsilon_psp_hum_mktg.eps_base_indiv_hum_vw  where AMB_opt_status='I') b
           on a.abbvie_indiv_id=b.abbvie_indiv_id)c
       left join
   (select  abbvie_indiv_id, count( distinct resource) as Res_ct from hc_data_integration_cws.master_enrollment_temp2
    group by abbvie_indiv_id) d
    on c.abbvie_indiv_id=d.abbvie_indiv_id)x
where x.flag=1
/*4828514*/
/*4838703 old count*/


--select count(*) from hc_data_integration_cws.master_enrollment_CRM;

    /******************************************************************************************/
    /*******Treating 'UNKNOWN' Channels to assign the value of channel corresponding to resource
    ************and their minimum enrollment_date********/
    /******************************************************************************************/
    
/************************************************************************/
/***Creating channels for Med_Reminder on minimum enrollment date*******/
/************************************************************************/

Create table hc_data_integration_cws.Med_Reminder_UNKNOWN_bckp as 
Select * from hc_data_integration_cws.Med_Reminder_UNKNOWN;
/*57561*/

Drop table hc_data_integration_cws.Med_Reminder_UNKNOWN;

create table hc_data_integration_cws.Med_Reminder_UNKNOWN as
select distinct
abbvie_indiv_id,
channel
from hc_data_integration_cws.Master_Enrollment_CRM
where upper(channel) = 'UNKNOWN'
and upper(resource) ='MED REMINDER';
/*58397 */


Create table hc_data_integration_cws.Med_Reminder_KNOWN_bckp as 
Select * from hc_data_integration_cws.Med_Reminder_KNOWN;
/*42382 */
Drop table hc_data_integration_cws.Med_Reminder_KNOWN;

create table hc_data_integration_cws.Med_Reminder_KNOWN as
select 
    abbvie_indiv_id,
    channel,
    enrollment_date
from 
(
select 
    abbvie_indiv_id,
    channel,
    enrollment_date,
    row_number() over (partition by abbvie_indiv_id order by enrollment_date asc) as min_date
from hc_data_integration_cws.Master_Enrollment_CRM
where upper(resource) ='MED REMINDER'
) a
where min_date = 1
and upper(channel) <> 'UNKNOWN'
;
Select distinct channel,resource from hc_data_integration_cws.Master_Enrollment_CRM;
/*43558*/
/*43520*/

Drop table hc_data_integration_cws.Med_rem_temp;

create table hc_data_integration_cws.Med_rem_temp as
select 
a.abbvie_indiv_id,
b.channel
from hc_data_integration_cws.Med_Reminder_UNKNOWN a 
inner join hc_data_integration_cws.Med_Reminder_KNOWN b 
on a.abbvie_indiv_id = b.abbvie_indiv_id;
/*32282 */
/*32304 old count*/
Drop table hc_data_integration_cws.Master_Enrollment_temp3;
create table hc_data_integration_cws.Master_Enrollment_temp3_bckp as 
Select * from hc_data_integration_cws.Master_Enrollment_temp3;
/*4617308*/
create table hc_data_integration_cws.Master_Enrollment_temp3 as
select 
m.abbvie_indiv_id,
m.resource,
m.subresource,
case    
    when upper(m.resource) ='MED REMINDER' and upper(m.channel_1) = 'UNKNOWN' then M.channel_2
    else m.channel_1
end as channel,
enrollment_date
from 
(
select 
a.abbvie_indiv_id,
resource,
subresource,
a.channel as channel_1,
b.channel as channel_2,
enrollment_date
from hc_data_integration_cws.Master_Enrollment_CRM a
left join hc_data_integration_cws.Med_rem_temp b 
on 
a.abbvie_indiv_id = b.abbvie_indiv_id )M
;
Select distinct channel from hc_data_integration_cws.Master_Enrollment_temp3
/*4828514*/

/************************************************************************/
/***Creating channels for Ambassador on minimum enrollment date*******/
/************************************************************************/
Drop table hc_data_integration_cws.Ambassador_UNKNOWN;

create table hc_data_integration_cws.Ambassador_UNKNOWN as
select distinct
abbvie_indiv_id,
channel
from hc_data_integration_cws.Master_Enrollment_temp3
where upper(channel) = 'UNKNOWN'
and upper(resource) ='AMBASSADOR';
/*602949*/
/*602930* old count*/
Drop table hc_data_integration_cws.Ambassador_KNOWN;

create table hc_data_integration_cws.Ambassador_KNOWN as
select 
    abbvie_indiv_id,
    channel,
    enrollment_date
from 
(
select 
    abbvie_indiv_id,
    channel,
    enrollment_date,
    row_number() over (partition by abbvie_indiv_id order by enrollment_date asc) as min_date
from hc_data_integration_cws.Master_Enrollment_temp3
where upper(resource) ='AMBASSADOR'
) a
where min_date = 1
and upper(channel) <> 'UNKNOWN'
;

/*383095*/
Select * from hc_data_integration_cws.Ambassador_KNOWN;

Drop table hc_data_integration_cws.Ambassador_temp;

create table hc_data_integration_cws.Ambassador_temp as
select 
a.abbvie_indiv_id,
b.channel
from hc_data_integration_cws.Ambassador_UNKNOWN a 
inner join hc_data_integration_cws.Ambassador_KNOWN b 
on a.abbvie_indiv_id = b.abbvie_indiv_id;
/*380786*/

Drop table hc_data_integration_cws.Master_Enrollment_temp4;

create table hc_data_integration_cws.Master_Enrollment_temp4 as
select 
m.abbvie_indiv_id,
m.resource,
m.subresource,
case    
    when upper(m.resource) ='Ambassador' and upper(m.channel_1) = 'UNKNOWN' then channel_2
    else m.channel_1
end as channel,
enrollment_date
from 
(
select 
a.abbvie_indiv_id,
resource,
subresource,
a.channel as channel_1,
b.channel as channel_2,
enrollment_date
from hc_data_integration_cws.Master_Enrollment_temp3 a
left join hc_data_integration_cws.Ambassador_temp b 
on 
a.abbvie_indiv_id = b.abbvie_indiv_id )M
;
/*4838703*/

/************************************************************************/
/***Creating channels for all resources on minimum enrollment date*******/
/************************************************************************/
Drop table hc_data_integration_cws.Unknown_channels;

create table hc_data_integration_cws.Unknown_channels as
select distinct
abbvie_indiv_id,
channel
from hc_data_integration_cws.Master_Enrollment_temp4
where upper(channel) = 'UNKNOWN'
or upper(channel) is NULL;
/*705762*/
Drop table hc_data_integration_cws.known_channels_start;

create table hc_data_integration_cws.known_channels_start as
select a.*  
from hc_data_integration_cws.Master_Enrollment_temp4 a
where (upper(a.channel)= 'AMBASSADOR'
or upper(a.channel)= 'APP' or upper(a.channel)='HCP' or upper(a.channel)='PHONE' or
upper(a.channel)='WEB' or upper(a.channel)='TEXT' or upper(a.channel)='OTHER');
/*2865892*/
Drop table hc_data_integration_cws.known_channels;

create table hc_data_integration_cws.known_channels as
select 
    abbvie_indiv_id,
    channel,
    enrollment_date
from 
(
select 
    abbvie_indiv_id,
    channel,
    enrollment_date,
    row_number() over (partition by abbvie_indiv_id order by enrollment_date asc,channel desc ) as min_date
from hc_data_integration_cws.known_channels_start
) a
where a.min_date = 1;
/*734209*/

Drop table hc_data_integration_cws.k_channels_temp;

create table hc_data_integration_cws.k_channels_temp as
select 
a.abbvie_indiv_id,
b.channel
from hc_data_integration_cws.unknown_channels a 
inner join hc_data_integration_cws.known_channels b 
on a.abbvie_indiv_id = b.abbvie_indiv_id;
/*565253*/

/************************************************************************/
/**************Creating final enrollment table**************************************/
/************************************************************************/

drop table hc_data_integration_cws.Master_Enrollment_backup_v1_test;

create table hc_data_integration_cws.1_Master_Enrollment_backup_v1_test as
select * from hc_data_integration_cws.Master_Enrollment_test_1_test;

Drop table hc_data_integration_cws.Master_Enrollment_test_1;

create table hc_data_integration_cws.Master_Enrollment_test_1 as
select 
distinct
n.abbvie_indiv_id,
n.resource,
case 
    when n.resource='RELATIONSHIP MARKETING' and n.subresource= 'RELATIONSHIP MARKETING' then 'SMS'
    else n.subresource
end as subresource,
case    
    when (upper(n.channel) is NULL or upper(n.channel)='UNKNOWN') and n.resource='MED REMINDER' then 'PHONE'
    when upper(n.channel) is NULL then 'UNKNOWN'
        else n.channel
end as channel,
n.enrollment_date 
from 
(
select 
m.abbvie_indiv_id,
m.resource,
m.subresource,
case    
    when upper(m.channel1) = 'UNKNOWN' or upper(m.channel1) is NULL then m.channel2
    else m.channel1
end as channel,
enrollment_date
from (
select 
a.abbvie_indiv_id,
resource,
subresource,
a.channel as channel1,
b.channel as channel2,
enrollment_date
from hc_data_integration_cws.Master_Enrollment_temp4 a
left join hc_data_integration_cws.k_channels_temp b 
on 
a.abbvie_indiv_id = b.abbvie_indiv_id
--EXCLUDING RESOURCES THAT FLOW IN DUE TO NON REMOVAL. THESE ARE EXCLUDED BY ZS.
where upper(resource) not in ('MED REM PHONE','MED REMINDER EM OPT OUT','MED REMINDER PATIENT APP',
'MED REMINDER PH OPT OUT','MED REMINDER TX OPT OUT','INJECTION TRAINING STATUS')
) M
) n;


/***********************************************************************/
/**************Quality Checks ****************/
/***********************************************************************/

/*SUMMARY AT ENROLLMENT AND YEAR LEVEL*/

select subresource, channel, year(enrollment_date) as YR, count(*) as REC_CNT, count(distinct abbvie_indiv_id) as pat_cnt from hc_data_integration_cws.1_Master_Enrollment_backup_v1
group by subresource, channel, year(enrollment_date)

select resource, year(enr_dt), month(enr_dt), count(*) from (
    select Resource, abbvie_indiv_id, min(enrollment_date) as enr_dt from
    (
    select * from 
    hc_data_integration_cws.1_Master_Enrollment
) y 
    group by
    resource, abbvie_indiv_id
    ) v
    group by resource, year(enr_dt), month(enr_dt)


select b.abbvie_indiv_id,count(b.abbvie_indiv_id),b.channel from hc_data_integration_cws.1_Master_Enrollment b
where b.abbvie_indiv_id in (select a.abbvie_indiv_id from hc_data_integration_cws.1_Master_Enrollment a
where a.channel is null)
group by b.abbvie_indiv_id,b.channel
having count(b.abbvie_indiv_id) > 1

-----required
select Resource, SubResource, channel, year(Enrollment_date) as Year, month(Enrollment_date)as Month,count(*) as Rec_C, count(distinct abbvie_indiv_id) as PAT_C
from hc_data_integration_cws.1_Master_Enrollment
group by Resource, SubResource, channel, year(Enrollment_date),month(Enrollment_date)
order by resource, subresource,channel,year, month;


select Resource, tactic, action,outcome ,count(*) as Rec_C, count(distinct abbvie_indiv_id) as PAT_C
from hc_data_integration_cws.1_Master_Fulfillment
group by Resource, tactic, action, outcome;

select count(distinct abbvie_indiv_id) from 1_master_enrollment 
where YEAR(enrollment_date) =2018 AND MONTH(enrollment_date) = 05 AND Resource like '%SHARPS%'

/**************************************************************************************/
/**********************************ENROLLMENT WIDE FORMAT******************************/
/**************************************************************************************/

/***************************************************************************/
/****Considering active_patient from both the Enrollment and Fulfillment tables
/****applying a filter of 4 dayS PRIOR to Current date, similiar thing has been done for previous year 
/***************************************************************************/
drop table hc_data_integration_cws.active_patient;

create table hc_data_integration_cws.active_patient as
select abbvie_indiv_id, max(activity_date) as ACT_DATE
from 
(
select abbvie_indiv_id, max(enrollment_date) as activity_date
from hc_data_integration_cws.Master_Enrollment_test_1
where TO_DATE(from_unixtime(UNIX_TIMESTAMP(enrollment_date, 'yyyy-MM-dd')))  <= date_sub(current_timestamp(),4)  AND
TO_DATE(from_unixtime(UNIX_TIMESTAMP(enrollment_date, 'yyyy-MM-dd')))  >= date_sub(current_timestamp(),369)
group by abbvie_indiv_id

union all

select abbvie_indiv_id, max(fullfillment_date) as activity_date
from hc_data_integration_cws.master_fulfillment_1
where TO_DATE(from_unixtime(UNIX_TIMESTAMP(fullfillment_date, 'yyyy-MM-dd')))  <= date_sub(current_timestamp(),4) AND
TO_DATE(from_unixtime(UNIX_TIMESTAMP(fullfillment_date, 'yyyy-MM-dd')))  >= date_sub(current_timestamp(),369) 
group by abbvie_indiv_id
) A
GROUP BY abbvie_indiv_id;
Select * from hc_data_integration_cws.Master_Enrollment_test_1_test

/***********************working of wide table **********************************************/
drop table hc_data_integration_cws.master_enrollment_temp1;

create table hc_data_integration_cws.master_enrollment_temp1 as
select 
    abbvie_indiv_id, 
    resource,
    min(enrollment_date) as enrollment_date
from hc_data_integration_cws.Master_Enrollment_test_1_test
group by abbvie_indiv_id, resource;
/*3033647*/

drop table hc_data_integration_cws.master_enrollment_wide_temp1;

create table hc_data_integration_cws.master_enrollment_wide_temp1 as
select
    b.abbvie_indiv_id,
    max(ambassador) as ambassador,
    max(app) as app,
    max(crm) as crm,
    max(cooler_bag) as cooler_bag,
    max(savings_card_optin) as savings_card_optin,
    max(injection_training_kit) as injection_training_kit,
    max(med_reminder) as med_reminder,
    max(nurse_call_center) as nurse_call_center,
    max(nurse_injection_training) as nurse_injection_training,
    max(sharps) as sharps,
    sum(case
            when ambassador is not null then 1
            when app is not null then 1
            when crm is not null then 1
            when cooler_bag is not null then 1
            when savings_card_optin is not null then 1
            when Injection_Training_kit  is not null then 1
            when med_reminder is not null then 1
            when nurse_call_center is not null then 1
            when nurse_injection_training is not null then 1
            when sharps is not null then 1
            else 0 
        end) as Resource_count
from
    (
    select 
        abbvie_indiv_id,
        case when resource = 'AMBASSADOR' then enrollment_date else '' end as ambassador,
        case when resource = 'PATIENT APP' then enrollment_date else '' end as App,
        case when resource = 'RELATIONSHIP MARKETING' then enrollment_date else '' end as CRM,
        case when resource = 'COOLER BAG' then enrollment_date else '' end as Cooler_bag,
        case when resource = 'SAVINGS CARD OPT-IN' then enrollment_date else '' end as Savings_Card_Optin,
        case when resource = 'INJECTION TRAINING KIT' then enrollment_date else '' end as Injection_Training_Kit,
        case when resource = 'MED REMINDER' then enrollment_date else '' end as Med_Reminder,
        case when resource = 'CALL CENTER' then enrollment_date else '' end as Nurse_Call_Center,
        case when resource = 'NURSE INJECTION TRAINING' then enrollment_date else '' end as Nurse_Injection_Training,
        case when resource = 'SHARPS' then enrollment_date else '' end as Sharps
    from hc_data_integration_cws.master_enrollment_temp1_test 
    ) b
    group by abbvie_indiv_id;

/*867332*/
/* Transposing the miminum enrollment dates per service for each patient and pulling other relevant metrics from other supoporting tables */
drop table hc_data_integration_cws.master_enrollment_wide_temp2;

create table hc_data_integration_cws.master_enrollment_wide_temp2 as
select 
    a.abbvie_indiv_id,
    a.ambassador, 
    a.app, 
    a.crm, 
    a.cooler_bag,
    a.savings_card_optin,
    a.injection_training_kit,
    a.med_reminder, 
    a.nurse_call_center, 
    a.nurse_injection_training, 
    a.sharps, 
    a.resource_count, 
    c.hc_optin_date, 
    case 
        when d.age IN ('', 'NULL') or d.age is null then 'UNKNOWN' 
        else UPPER(d.age) 
    end as age,  
    case 
        when d.gndr_cd IN ('', 'NULL', 'U') or d.gndr_cd is null then 'UNKNOWN' 
        else UPPER(d.gndr_cd) 
    end as gndr_cd, 
    h.zip_base_cd,
    d.thrpy_ini_date, 
    UPPER(d.amb_opt_status) as amb_opt_status, 
    UPPER(x.hc_opt_status) as hc_opt_status,
    case
        when upper(AMB_IND) = 'PSA DERM' THEN 
            case 
                when e.indication is not null then upper(e.indication)
                else 'PSA'
            end
        else UPPER(COALESCE(AMB_IND, e.indication)) 
    end as Indication,
    UPPER(e.indication) as Indication_ESP, 
    case    
        when UPPER(e.indication) IN ('RA', 'AS', 'JIA', 'UV') OR (UPPER(e.indication) = 'PSA' AND UPPER(K.PAT_SPEC) = 'PATIENT - RHEUM') then 'RHEUM'
        when UPPER(e.indication) IN ('PS', 'HS') OR (UPPER(e.indication) = 'PSA' AND UPPER(K.PAT_SPEC) = 'PATIENT - DERM') then 'DERM'
        when UPPER(e.indication) IN ('UC', 'CD', 'PCD') then 'GASTRO' else 'UNKNOWN' 
    end as Franchise,
    f.opt_out_hc as opt_out_hc,
    case when length(UPPER(g.opt_out_amb))>0 then UPPER(g.opt_out_amb) 
    else Null 
    end as opt_out_amb, 
    case 
    when length(UPPER(K.AMB_TYPE))>0 then UPPER(K.AMB_TYPE)
    else 'UNKNOWN'
    end as AMB_TYPE, 
    case 
    when length(UPPER(K.AMB_IND))>0 then UPPER(K.AMB_IND)
    else 'UNKNOWN' 
    end as AMB_IND, 
    case 
    when length(UPPER(K.time_on_drug_at_enrollment))>0 then UPPER(K.time_on_drug_at_enrollment) 
    else 'UNKNOWN'
    end as time_on_drug_at_enrollment, 
    case 
    when length(UPPER(K.PAT_SPEC))>0 then UPPER(K.PAT_SPEC) 
    else 'UNKNOWN' 
    end as PAT_SPEC, 
    n.CHANNEL,
    case 
    when length(UPPER(K.HCP_SPEC))>0 then UPPER(K.HCP_SPEC) 
    else 'UNKNOWN' 
    end as HCP_SPEC, 
    CONCAT(gndr_cd,' ',age) as Segment,
    k.therapy_end_date, 
    case 
        when length(UPPER(k.initial_drug_experience))>0 then UPPER(k.initial_drug_experience) 
        when  UPPER(k.initial_drug_experience) = 'NULL'  then 'UNKNOWN' else 'UNKNOWN' 
    end as initial_drug_experience, 
    case 
        when length(UPPER(k.enrollment_status))>0 then UPPER(k.enrollment_status) 
        when  UPPER(k.enrollment_status) = 'NULL'  then 'UNKNOWN' 
        when  UPPER(k.enrollment_status) is Null  then 'UNKNOWN'
        else 'UNKNOWN' 
    end as AMB_TYPE_QNI, 
    case
        when m.abbvie_indiv_id is NOT NULL then 'Y' 
        else 'N' 
    end as ACT_PAT,
    k.account_record_type,
    k.DRUG_EDUCATION,
    k.DISEASE_EDUCATION,
    k.INSURANCE_SUPPORT,
    k.INJECTION_TRAINING,
    k.master_activity_type
from hc_data_integration_cws.master_enrollment_wide_temp1_test a

    left join
        (select abbvie_indiv_id, min(enrollment_date) as HC_Optin_date
        from hc_data_integration_cws.master_enrollment_temp1_test 
        group by abbvie_indiv_id) c
        on a.abbvie_indiv_id = c.abbvie_indiv_id
    
    left join
        (select abbvie_indiv_id,age as age, gndr_cd,  thrpy_ini_date, amb_opt_status, hc_opt_status, 	physician_zip
        from epsilon_psp_hum_mktg.eps_base_indiv_hum_vw
        ) d
        on a.abbvie_indiv_id = d.abbvie_indiv_id
    
    left  join
        (select abbvie_indiv_id, indication
        from (
        select a.abbvie_indiv_id, b.indication, b.vendor_code, row_number() OVER (PARTITION BY abbvie_indiv_id
        ORDER BY
        CASE
          WHEN vendor_code = 'QINT' THEN 'A'
          WHEN vendor_code = 'LVP' THEN 'B'
          WHEN vendor_code = 'DIG ' THEN 'C'
          ELSE 'D' END) AS priority
        from 
        (select abbvie_indiv_id, max(rsp_dt) as response_date from epsilon_psp_hum_mktg.eps_indication_hum_vw
        where indication is not NULL group by abbvie_indiv_id) a
        left join (select distinct abbvie_indiv_id, rsp_dt, indication, vendor_code
        from epsilon_psp_hum_mktg.eps_indication_hum_vw) b on
        a.abbvie_indiv_id = b.abbvie_indiv_id and a.response_date = b.rsp_dt
        group by a.abbvie_indiv_id, b.indication, b.vendor_code
        ) x where priority = 1
        ) e on a.abbvie_indiv_id = e.abbvie_indiv_id
        
        left join
        (select abbvie_indiv_id, max(opt_ts) as opt_out_hc from epsilon_psp_hum_mktg.eps_indiv_opt_hist_hum_vw
            where opt_type = 'HC' and opt_val_cd = 'O'
            group by abbvie_indiv_id
        ) f
        on a.abbvie_indiv_id = f.abbvie_indiv_id
        
        left join
        (select abbvie_indiv_id, opt_val_cd as hc_opt_status from epsilon_psp_hum_mktg.eps_indiv_opt_hist_hum_vw
            where opt_type = 'HC'
            group by abbvie_indiv_id, opt_val_cd
        ) x
        on a.abbvie_indiv_id = x.abbvie_indiv_id

        left join
        (select abbvie_indiv_id, max(opt_ts) as opt_out_amb from epsilon_psp_hum_mktg.eps_indiv_opt_hist_hum_vw
            where opt_type = 'AMB' and opt_val_cd = 'O'
            group by abbvie_indiv_id
        ) g
        on a.abbvie_indiv_id = g.abbvie_indiv_id
        
                left join
        (select abbvie_indiv_id,zip_base_cd
        from epsilon_psp_hum_mktg.eps_base_indiv_hum_vw
        ) h
        on a.abbvie_indiv_id = h.abbvie_indiv_id
        
         left join
         (select abbvie_indiv_id, SubResource as AMB_TYPE, 
         case 
            when UPPER(indication_name) = 'PS & PSA' then 'PSA DERM'
            else UPPER(substr(indication_name,
            instr(indication_name,'(')+1, length(indication_name)-instr(indication_name,'(')-1)) 
        end as AMB_IND, 
         account_record_type as PAT_SPEC,account_record_type, CHANNEL, abbott_best_specialty_code as HCP_SPEC,
         therapy_end_date, initial_drug_experience, enrollment_status, time_on_drug_at_enrollment,
         DRUG_EDUCATION,
         DISEASE_EDUCATION,
         INSURANCE_SUPPORT,
         INJECTION_TRAINING,
         master_activity_type
            from
            (
            select distinct abbvie_indiv_id,  SubResource, channel,
            case when upper(indication_name) = 'Z' then 'UNKNOWN'
            else indication_name
            end as indication_name, account_record_type, abbott_customer_id, therapy_end_date, 
            initial_drug_experience, enrollment_status, time_on_drug_at_enrollment, 
            DRUG_EDUCATION,
            DISEASE_EDUCATION,
            INSURANCE_SUPPORT,
            INJECTION_TRAINING,
            master_activity_type
            from (select abbvie_indiv_id,  SubResource, channel, 
            case when upper(trim(indication_name)) in ('','Null', NULL) then 'Z'
            else indication_name
            end as indication_name, account_record_type, abbott_customer_id, therapy_end_date, initial_drug_experience,
            enrollment_status, time_on_drug_at_enrollment,Enrollment_date,
            DRUG_EDUCATION,
            DISEASE_EDUCATION,
            INSURANCE_SUPPORT,
            INJECTION_TRAINING,
            master_activity_type,
            RANK() OVER (PARTITION BY abbvie_indiv_id ORDER BY Enrollment_date DESC, indication_name) AS RANK
                from hc_data_integration_cws.hc_ambassador_activity_refined
                where abbvie_indiv_id is not null) H
            WHERE H.RANK = 1 
            ) I
            left join (select * from master_Data.customer_tbl where dds_active_flag = 'Y') J
                ON I.abbott_customer_id = J.abbott_customer_id
        ) K
                on a.abbvie_indiv_id = k.abbvie_indiv_id
        left join
        (select abbvie_indiv_id  from hc_data_integration_cws.active_patient
        ) m
        on a.abbvie_indiv_id = m.abbvie_indiv_id
        
        left join 
        (select 
            abbvie_indiv_id,
            channel,
            enrollment_date
        from 
        (
        select 
            abbvie_indiv_id,
            channel,
            enrollment_date,
            row_number() over (partition by abbvie_indiv_id order by enrollment_date asc,channel desc ) as min_date
        from hc_data_integration_cws.1_MASTER_enrollment
        ) a
        where a.min_date = 1
        ) n
        on a.abbvie_indiv_id = n.abbvie_indiv_id;

--set hive.auto.convert.join.noconditionaltask=false;


/*******************************QC*************************************/
select abbvie_indiv_id from hc_data_integration_cws.master_enrollment_wide_temp2
where  UPPER(INDICATION) = 'PSA';
--76,093
select abbvie_indiv_id from hc_data_integration_cws.1_Master_Enrollment_wide_new
where  UPPER(INDICATION) = 'PSA' and upper(indication_old) = 'UNKNOWN';

select * from hc_data_integration_cws.master_enrollment_wide_temp2 where INJECTION_TRAINING=1;

select x.abbvie_indiv_id,y.indication as indic_id, a.indication as amb_id from 
(
select distinct abbvie_indiv_id, indication from 1_Master_Enrollment_wide_V1 where indication ='UNKNOWN') x
left join
(
select distinct abbvie_indiv_id, indication from epsilon_psp_hum_mktg.eps_indication_hum_vw) y on x.abbvie_indiv_id=y.abbvie_indiv_id
left join (
select distinct abbvie_indiv_id, indication_name as indication from hc_data_integration_cws.hc_ambassador_activity_refined) a
on x.abbvie_indiv_id=a.abbvie_indiv_id

select distinct  
         case 
            when UPPER(indication_name) = 'PS & PSA' then 'PSA DERM'
            when instr(indication_name,'(')>0 then UPPER(substr(indication_name,
            instr(indication_name,'(')+1, length(indication_name)-instr(indication_name,'(')-1)) 
        end as AMB_IND
        from hc_data_integration_cws.hc_ambassador_activity_refined


--*******************************************************************************

DROP TABLE hc_data_integration_cws.indication_SUM;

create table hc_data_integration_cws.indication_SUM as 
select 
B.abbvie_indiv_id,
SUM(indication_RHEUM) AS SUM_R,
SUM(indication_DERM) AS SUM_D,
SUM(indication_GASTRO) AS SUM_G,
SUM(indication_PSA) AS SUM_PSA
FROM (
SELECT 
abbvie_indiv_id,
case when UPPER(indication) ='AS' THEN 1 
    when UPPER(indication) = 'RA' THEN 1
    when UPPER(indication) = 'JIA' THEN 1
    when UPPER(indication) = 'UV' THEN 1
    ELSE 0
END As indication_RHEUM,
case 
    when UPPER(indication) ='PS' THEN 1 
    when UPPER(indication) = 'HS' THEN 1
    ELSE 0
END As indication_DERM,
case 
    when UPPER(indication) IN ('UC', 'CD', 'PCD') THEN 1
    ELSE 0
END As indication_GASTRO,
case 
    when UPPER(indication) ='PSA' THEN 1 
    ELSE 0
END As indication_PSA
from epsilon_psp_hum_mktg.eps_indication_hum_vw) B
GROUP BY ABBVIE_INDIV_ID;
/*1229802*/

/********************************************************/
/******************QC CHECKS*****************************/
/********************************************************/
select abbvie_indiv_id, count(abbvie_indiv_id)
from hc_data_integration_cws.indication_SUM
group by abbvie_indiv_id
having count(abbvie_indiv_id) >1

SELECT ABBVIE_INDIV_ID FROM hc_data_integration_cws.indication_SUM
WHERE SUM_R >=1 AND SUM_D>=1 AND SUM_G >=1 AND SUM_PSA >=1 

/********************END OF QC CHECK**********************************/



/*******************Code for CRM Rheum derm PSA indication *******************/

drop table hc_data_integration_cws.crm_max_dtinfo ;

create table hc_data_integration_cws.crm_max_dtinfo as 
select 
abbvie_indiv_id,
max(extract_date) as extract_date
from epsilon_psp_hum_mktg.eps_crm_hum_vw
where UPPER(kit_nm) like '%RHEUM%' OR UPPER(kit_nm) like '%DERM%'
group by abbvie_indiv_id ;
/*670916*/

drop table hc_data_integration_cws.crm_max_dt;

create table hc_data_integration_cws.crm_max_dt as
select
c.abbvie_indiv_id,
c.extract_date,
c.RH_DRM_INFO
from 
(
select 
a.abbvie_indiv_id,
a.extract_date,
CASE 
WHEN UPPER(b.kit_nm)  like '%RHEUM%' THEN 'A-RHEUM'
WHEN UPPER(b.kit_nm)  like '%DERM%' THEN 'B-DERM'
END AS RH_DRM_INFO
from hc_data_integration_cws.crm_max_dtinfo_test a
left join epsilon_psp_hum_mktg.eps_crm_hum_vw b 
on a.abbvie_indiv_id = b.abbvie_indiv_id
and a.extract_date = b.extract_date
where UPPER(b.kit_nm) like '%RHEUM%' or UPPER(b.kit_nm)  like '%DERM%'
) c
where c.RH_DRM_INFO is not null
;
/*688279*/
drop table hc_data_integration_cws.crm_info_final;

create table hc_data_integration_cws.crm_info_final as 
select 
b.abbvie_indiv_id,
b.extract_date,
b.rhdm_info
from 
(
select 
a.abbvie_indiv_id,
a.extract_date,
a.RH_DRM_INFO as rhdm_info,
row_number() over (partition by a.abbvie_indiv_id, a.extract_date order by a.RH_DRM_INFO) as prior
from hc_data_integration_cws.crm_max_dt a) b
    where prior = 1;
    /*670916*/

/**********************end of crm rheum derm psa indication ******************************/

/******************************Creating final wide table ***************************/
drop table hc_data_integration_cws.1_Master_Enrollment_wide_V1;

create table hc_data_integration_cws.Master_Enrollment_wide_V1_1 as
select distinct 
    m.abbvie_indiv_id,	
    m.ambassador,
    m.app,
    m.crm,
    m.cooler_bag,
    m.savings_card_optin,
    m.injection_training_kit,  
    m.med_reminder,
    m.nurse_call_center	,
    m.nurse_injection_training,
    m.sharps,
    case 
         when m.DRUG_EDUCATION in ('','Null') or m.DRUG_EDUCATION is Null then 0
         ELSE m.DRUG_EDUCATION
    end as DRUG_EDUCATION,
    case 
         when m.DISEASE_EDUCATION in ('','Null') or m.DISEASE_EDUCATION is Null then 0
         ELSE m.DISEASE_EDUCATION
    end as DISEASE_EDUCATION,
    case 
         when m.INSURANCE_SUPPORT in ('','Null') or m.INSURANCE_SUPPORT is Null then 0
         ELSE m.INSURANCE_SUPPORT
    end as INSURANCE_SUPPORT,
    case 
         when m.INJECTION_TRAINING in ('','Null') or m.INJECTION_TRAINING is Null then 0
         ELSE m.INJECTION_TRAINING
    end as INJECTION_TRAINING,
    m.resource_count,
    m.hc_optin_date,	
    case 
        when m.age in ('','Null') or m.age is NULL then 'UNKNOWN'
        ELSE m.age
    end as age,
    case 
        when m.gndr_cd in ('','Null') or m.gndr_cd is Null then 'UNKNOWN'
        ELSE m.gndr_cd
    end as gndr_cd,
    thrpy_ini_date,
    case 
        when m.amb_opt_status in ('','Null') or m.amb_opt_status is Null then 'UNKNOWN'
        ELSE m.amb_opt_status
    end as amb_opt_status,
    case 
        when m.hc_opt_status in ('','Null') or m.hc_opt_status is Null then 'UNKNOWN'
        ELSE m.hc_opt_status
    end as hc_opt_status,
    case 
        when upper(m.indication) in ('','NULL') or upper(m.indication) = 'NONE OF THE ABOV' or m.indication is NULL then 'UNKNOWN'
        ELSE m.indication
    end as indication,
    case 
        when m.indication_esp in ('','Null') or m.indication_esp is Null then 'UNKNOWN'
        ELSE m.indication_esp
    end as indication_esp,
    franchise as franchise_old,
    opt_out_hc,
    opt_out_amb,
    case 
        when m.amb_type in ('','Null') or m.amb_type is Null then 'UNKNOWN'
        ELSE m.amb_type
    end as amb_type,
    case 
        when m.amb_ind in ('','Null') or m.amb_ind is Null then 'UNKNOWN'
        ELSE m.amb_ind
    end as amb_ind,
    case 
        when Upper(m.time_on_drug_at_enrollment) in ('','NULL') or m.time_on_drug_at_enrollment is Null then 'UNKNOWN'
        ELSE m.time_on_drug_at_enrollment
    end as time_on_drug_at_enrollment,
    case 
        when m.pat_spec in ('','Null') or m.pat_spec is Null then 'UNKNOWN'
        ELSE m.pat_spec
    end as pat_spec,
    case 
        when m.channel in ('','Null') or m.channel is Null then 'UNKNOWN'
        ELSE m.channel
    end as channel,
    case 
        when m.hcp_spec in ('','Null') or m.hcp_spec is Null then 'UNKNOWN'
        ELSE m.hcp_spec
    end as hcp_spec,
    case 
        when m.segment in ('','Null') or m.segment is Null then 'UNKNOWN'
        ELSE m.segment
    end as segment,
    m.therapy_end_date,
    case 
        when Upper(m.initial_drug_experience) in ('','NULL') or m.initial_drug_experience is Null then 'UNKNOWN'
        ELSE m.initial_drug_experience
    end as initial_drug_experience,
    case 
        when Upper(m.amb_type_qni) in ('','NULL') or m.amb_type_qni is Null then 'UNKNOWN'
        ELSE m.amb_type_qni
    end as amb_type_qni,
    case 
        when m.act_pat in ('','Null') or m.act_pat is Null then 'UNKNOWN'
        ELSE m.act_pat
    end as act_pat,
    case 
        when m.zip_base_cd in ('','Null') or m.zip_base_cd is Null then 'UNKNOWN'
        ELSE m.zip_base_cd
    end as zip_base_cd,
    indication_mod as indication_old,
    case 
        when UPPER(m.indication) in ('RA', 'AS', 'JIA', 'UV') then 'RHEUM'
        when UPPER(m.indication) in ('PS', 'HS') then 'DERM'
        when UPPER(m.indication) in ('UC', 'CD', 'PCD') then 'GASTRO'
        WHEN UPPER(M.ACCOUNT_record_type) like '%RHEUM%' then 'RHEUM'
        WHEN UPPER(M.ACCOUNT_record_type) like '%DERM%' then 'DERM'
        WHEN UPPER(M.ACCOUNT_record_type) like '%GASTRO%' THEN 'GASTRO'
        WHEN UPPER(M.ACCOUNT_record_type) like '%Juvenile Idiopathic Arthritis%' THEN 'RHEUM'
        WHEN UPPER(M.ACCOUNT_record_type) like '%Pediatric Crohn%' THEN 'GASTRO'
        when UPPER(m.indication_mod) = 'PSA DERM' then 'DERM'
        when UPPER(m.indication_mod) = 'PSA RHEUM' then 'RHEUM'
        when UPPER(m.indication_mod) = 'PSA GASTRO' then 'GASTRO'
        ELSE 'UNKNOWN'
    end as franchise
from 
(
select 
    a.abbvie_indiv_id,	
    a.ambassador,
    a.app,
    a.crm,
    a.cooler_bag,
    a.savings_card_optin,
    a.injection_training_kit,
    a.med_reminder,
    a.nurse_call_center	,
    a.nurse_injection_training,
    a.sharps,
    a.DRUG_EDUCATION,
    a.DISEASE_EDUCATION,
    a.INSURANCE_SUPPORT,
    a.INJECTION_TRAINING,
    a.resource_count,
    a.hc_optin_date,	
    a.age,
    a.gndr_cd,
    a.thrpy_ini_date,
    a.amb_opt_status,
    a.hc_opt_status,
    a.indication,
    a.indication_esp,
    case 
        when a.franchise in ('','Null') or a.franchise is Null then 'UNKNOWN'
        ELSE a.franchise
    end as franchise,
    a.opt_out_hc,
    a.opt_out_amb,
    a.amb_type,
    a.amb_ind,
    a.time_on_drug_at_enrollment,
    a.pat_spec,
    a.channel,
    a.hcp_spec,
    a.segment,
    a.therapy_end_date,
    a.initial_drug_experience,
    a.amb_type_qni,
    a.act_pat,
    a.zip_base_cd,
    a.account_record_type,
    
--***********************************************************************/
-- Indication from AMB otherwise from Indication table
-- If Indication is PSA then
-- 1. Update PSA Indication based on PAT_SPEC 
-- 2. Update PSA Indication based on PHY_SPEC
-- 3. Update PSA Indication based on Franchise Rule; Rheum will take priority
-- 4. Update based on CRM Kit (Kit_Name)
-- 5. PSA - UNKNOWN
--***********************************************************************/
    case 
        when UPPER(indication) <> 'PSA' then indication
        else
            case
                when UPPER(indication) = 'PSA' AND UPPER(b.specialty_DESCRIPTION) like '%RHEUM%' then 'PSA RHEUM'
                when UPPER(indication) = 'PSA' AND UPPER(b.specialty_DESCRIPTION) like '%DERM%' then 'PSA DERM'
                when UPPER(indication) = 'PSA' AND UPPER(b.specialty_DESCRIPTION) like '%GASTRO%' then 'PSA GASTRO'               
                when UPPER(indication) = 'PSA' AND UPPER(PAT_SPEC) = 'RHEUMATOLOGY' then 'PSA RHEUM'
                when UPPER(indication) = 'PSA' AND UPPER(PAT_SPEC) = 'DERMATOLOGY' then 'PSA DERM'
                when UPPER(indication) = 'PSA' AND UPPER(PAT_SPEC) like '%GASTRO%' then 'PSA GASTRO'

                when  SUM_R >=1 AND SUM_PSA >=1 THEN 'PSA RHEUM'
                when  SUM_D >=1 AND SUM_PSA >=1 THEN 'PSA DERM'
                when  SUM_G >=1 AND SUM_PSA >=1 THEN 'PSA GASTRO'
                when  SUM_R >=1 AND SUM_D >=1 AND SUM_PSA >=1 THEN 'PSA RHEUM'
                when UPPER(indication) = 'PSA' AND UPPER(rhdm_info) like '%RHEUM%' then 'PSA RHEUM'
                when UPPER(indication) = 'PSA' AND UPPER(rhdm_info) like '%DERM%' then 'PSA DERM'
                when UPPER(indication) = 'PSA' AND UPPER(rhdm_info) like '%GASTRO%' then 'PSA GASTRO'
                else 'UNKNOWN' 
            end
    end as Indication_mod
from hc_data_integration_cws.master_enrollment_wide_temp2 a 
left join
(select distinct specialty_code, specialty_DESCRIPTION from master_data.specialty_tbl) b 
on a.hcp_spec = b.specialty_code 
LEFT JOIN 
(select abbvie_INDIV_id,sum_r,sum_d,sum_psa,SUM_G from hc_data_integration_cws.indication_sum) c
ON a.abbvie_INDIV_id=c.abbvie_INDIV_id
left join 
(select distinct abbvie_indiv_id,rhdm_info from hc_data_integration_cws.crm_info_final) D 
on a.abbvie_indiv_id = D.abbvie_indiv_id
) m;

drop table hc_data_integration_cws.1_master_enrollment_wide_backup_v1_test;

create table hc_data_integration_cws.1_master_enrollment_wide_backup_v1_test as
select * from hc_data_integration_cws.1_master_enrollment_wide;

drop table hc_data_integration_cws.1_master_enrollment_wide_test;

create table hc_data_integration_cws.1_master_enrollment_wide_test as
select b.* from (
select a.*, rank() over (Partition By a.abbvie_indiv_id order by a.RowNo) Rnk 
from (
  select 
        *, 
            row_number() over 
            (order by DRUG_EDUCATION, DISEASE_EDUCATION, INSURANCE_SUPPORT) RowNo 
        from hc_data_integration_cws.1_master_enrollment_wide_V1) a) b where Rnk=1;
/*711804*/


/***************QC CHECKS*************************/
select channel , count(channel)  from hc_data_integration_cws.1_master_enrollment_wide
group by channel

select count(distinct abbvie_indiv_id), count(*) from hc_data_integration_cws.1_master_enrollment_wide;
select count(*) as rec_cnt, count(abbvie_indiv_id) as pat_cnt from hc_data_integration_cws.1_master_enrollment_wide;

CREATE TABLE hc_data_integration_cws.QC_1 AS
select abbvie_indiv_id from hc_data_integration_cws.1_Master_Enrollment_wide_new
where UPPER(Indication_R) = 'UNKNOWN' AND UPPER(INDICATION) = 'PSA';

select amb_ind from 1_Master_Enrollment_wide
where upper(amb_ind) = 'UNKNOWN'

CREATE TABLE hc_data_integration_cws.QC_2 AS
select A.abbvie_indiv_id from hc_data_integration_cws.QC_1 A
WHERE A.abbvie_indiv_id IN (select B.abbvie_indiv_id from hc_data_integration_cws.crm_info_final B);

select distinct indication_name, 
case when UPPER(indication_name) = 'PS & PSA' then 'PSA DERM'
else UPPER(substr(indication_name,instr(indication_name,'(')+1, length(indication_name)-instr(indication_name,'(')-1)) end as indication_abbr
from hc_data_integration_cws.hc_ambassador_activity_refined 

select abbvie_indiv_id,hc_optin_date, indication from hc_data_integration_cws.1_Master_Enrollment_wide
where indication='UNKNOWN'

/**************************************************************************************/
/**********************************ENROLLMENT TALL & WIDE FORMAT TBL for QLIK Team******************************/
/**************************************************************************************/
drop table hc_data_integration_cws.Master_Enrollment_Report_2012_2014_test;     

 Create table hc_data_integration_cws.Master_Enrollment_Report_2012_2014_test
 as select * from hc_data_integration_cws.1_Master_Enrollment where resource ='AMBASSADOR' and 
 TO_DATE(from_unixtime(UNIX_TIMESTAMP(enrollment_date, 'yyyy-MM-dd'))) <= '2014-12-31';

drop table hc_data_integration_cws.Master_Enrollment_Report_TBL_test;

-- can be done without  group by also
create table hc_data_integration_cws.Master_Enrollment_Report_TBL_test as
select abbvie_indiv_id, resource,subresource as tactic, 
CASE WHEN upper(channel)="OTHERS" THEN "OTHER"
     ELSE upper(channel)
end as channel, 
min(enrollment_date) as enrollment_date
from hc_data_integration_cws.1_Master_Enrollment
--where TO_DATE(from_unixtime(UNIX_TIMESTAMP(enrollment_date, 'yyyy-MM-dd'))) >= '2015-01-01'
where year(enrollment_date) >= 2015
group by abbvie_indiv_id, resource,subresource,channel
UNION ALL
 select distinct abbvie_indiv_id, resource,subresource as tactic, channel, enrollment_date
 from hc_data_integration_cws.1_Master_Enrollment where resource ='AMBASSADOR' and 
 year(enrollment_date) < 2015
 --TO_DATE(from_unixtime(UNIX_TIMESTAMP(enrollment_date, 'yyyy-MM-dd'))) <= '2014-12-31';
 
drop table hc_data_integration_cws.Master_Enrollment_Report_temp_tbll_test;

create table hc_data_integration_cws.Master_Enrollment_Report_temp_tbll_test as
select abbvie_indiv_id, resource, min(enrollment_date) as enrollment_date
 from hc_data_integration_cws.1_Master_Enrollment
 group by abbvie_indiv_id, resource;

drop table hc_data_integration_cws.Master_Enrollment_wide_TBL_2_test;

create table hc_data_integration_cws.Master_Enrollment_wide_TBL_2_test as
select
b.abbvie_indiv_id
,max(ambassador) as ambassador
,max(app) as app
,max(crm) as crm
,max(cooler_bag) as cooler_bag
,max(savings_card_optin) as savings_card_optin
,max(injection_training_kit) as injection_training_kit
,max(med_reminder) as med_reminder
,max(nurse_call_center) as nurse_call_center
,max(nurse_injection_training) as nurse_injection_training
,max(sharps) as sharps
,sum (case when ambassador is not null then 1
    when app is not null then 1
    when crm is not null then 1
    when cooler_bag is not null then 1
    when savings_card_optin is not null then 1
    when Injection_Training_kit  is not null then 1
    when med_reminder is not null then 1
    when nurse_call_center is not null then 1
    when nurse_injection_training is not null then 1
    when sharps is not null then 1
    else 0 end) as Resource_count
from
    (select 
    abbvie_indiv_id,
    case when resource = 'AMBASSADOR' then enrollment_date else '' end as ambassador
    ,case when resource = 'PATIENT APP' then enrollment_date else '' end as App
    ,case when resource = 'RELATIONSHIP MARKETING' then enrollment_date else '' end as CRM
    ,case when resource = 'COOLER BAG' then enrollment_date else '' end as Cooler_bag
    ,case when resource = 'SAVINGS CARD OPT-IN' then enrollment_date else '' end as Savings_Card_Optin
    ,case when resource = 'INJECTION TRAINING KIT' then enrollment_date else '' end as Injection_Training_Kit
    ,case when resource = 'MED REMINDER' then enrollment_date else '' end as Med_Reminder
    ,case when resource = 'CALL CENTER' then enrollment_date else '' end as Nurse_Call_Center
    ,case when resource = 'NURSE INJECTION TRAINING' then enrollment_date else '' end as Nurse_Injection_Training
    ,case when resource = 'SHARPS' then enrollment_date else '' end as Sharps
    from hc_data_integration_cws.Master_Enrollment_Report_temp_tbll ) b
    group by abbvie_indiv_id;

drop table hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL_test;

create table hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL_test  as
select 
    a.abbvie_indiv_id,
    a.ambassador,
    a.app, 
    a.crm, 
    a.cooler_bag,
    a.savings_card_optin,
    a.injection_training_kit, 
    a.med_reminder, 
    a.nurse_call_center,
    a.nurse_injection_training,
    a.sharps,
    a.resource_count,
    CONCAT(l.gndr_cd,' ',l.age) as Segment, 
    l.ACT_PAT,
    l.age,
    l.gndr_cd,
    l.thrpy_ini_date,
    l.amb_opt_status,
    l.hc_opt_status,
    l.hc_optin_date,
    l.opt_out_hc,
    l.opt_out_amb,
    l.amb_type,
    l.amb_ind,
    l.pat_spec,
    CASE WHEN UPPER(l.channel)="OTHERS" THEN "OTHER"
         ELSE UPPER(l.channel)
    END AS channel,
    l.hcp_spec,
    l.zip_base_cd,
    l.indication,
    l.franchise,
    l.therapy_end_date,
    l.initial_drug_experience,
    l.amb_type_qni,
    l.DRUG_EDUCATION,
    l.DISEASE_EDUCATION,
    l.INSURANCE_SUPPORT,
    l.INJECTION_TRAINING
from hc_data_integration_cws.Master_Enrollment_wide_TBL_2 a
left join 
(select 
    abbvie_indiv_id,
    a.age,
    a.gndr_cd,
    a.thrpy_ini_date,
    a.amb_opt_status,
    a.hc_opt_status,
    a.hc_optin_date,
    a.opt_out_hc,
    a.opt_out_amb,
    a.amb_type,
    a.amb_ind,
    a.pat_spec,
    a.channel,
    a.hcp_spec,
    a.act_pat,
    a.zip_base_cd,
    indication,
    franchise,
    therapy_end_date,
    initial_drug_experience,
    amb_type_qni,
    a.DRUG_EDUCATION,
    a.DISEASE_EDUCATION,
    a.INSURANCE_SUPPORT,
    a.INJECTION_TRAINING
from hc_data_integration_cws.1_Master_Enrollment_wide a
) L
    on a.abbvie_indiv_id = l.abbvie_indiv_id;

/**************************************************************************************/
/**********************************QC Checks ******************************/
/**************************************************************************************/
--Testing Trend
select resource,  count(*) as REC_CNT, Count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.1_Master_Enrollment
group by resource;

select min(fullfillment_date), max(fullfillment_date)  from hc_data_integration_cws.Master_Fulfillment_report_tbl

select resource, subresource, channel, count(*) as REC_CNT, Count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.1_Master_Enrollment
group by resource, subresource, channel;

select distinct channel from 1_master_enrollment_wide
select  count(*) as REC_CNT, Count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL

select * from  hc_data_integration_cws.1_Master_Enrollment_wide limit 10;

select franchise_old, count(*) as REC_CNT, count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.1_Master_Enrollment_wide
GROUP BY franchise_old

select resource, channel, count(*) as REC_CNT, Count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.1_Master_Enrollment
group by resource, channel;

--Testing Unique Patient IDs - OFFSHORE TABLE
select count(*) as REC_CNT, count(distinct abbvie_indiv_id) as PAT_CNT from  hc_data_integration_cws.1_master_enrollment_wide;

--Testing Unique Patient IDs - QLIK TABLE
select count(*) as REC_CNT, count(distinct abbvie_indiv_id) as PAT_CNT from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL;

select * from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL where INJECTION_TRAINING ='1';

select distinct a.abbvie_indiv_id, ambassador_kpi from hc_data_integration_cws.1_Master_Enrollment_wide a
inner join 
(select distinct c.abbvie_indiv_id from 
(select abbvie_indiv_id, count(abbvie_indiv_id) as cnt from hc_data_integration_cws.1_Master_Enrollment_wide group by abbvie_indiv_id) c where cnt>1) b
on a.abbvie_indiv_id=b.abbvie_indiv_id;

select max(enrollment_date) from hc_data_integration_cws.Master_Enrollment_Report_TBL;

select distinct "AMB" as tit, year(ambassador) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "APP" as tit, year(app) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "CRM" as tit, year(crm) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "COOL" as tit, year(cooler_bag) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "OPUS" as tit, year(savings_card_optin) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "ITK" as tit, year(injection_training_kit) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "MED" as tit, year(med_reminder) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "CALL" as tit, year(nurse_call_center) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "NIT" as tit, year(nurse_injection_training) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "SHARp" as tit, year(sharps) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "thrpy_ini_date" as tit, year(thrpy_ini_date) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "hc_optin_date" as tit, year(hc_optin_date) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "opt_out_hc" as tit, year(opt_out_hc) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL union all
select distinct "opt_out_amb" as tit, year(opt_out_amb) from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL

select distinct channel from hc_data_integration_cws.Master_enrollment_Report_TBL;

select resource, count(*) from hc_data_integration_cws.Master_fulfillment_Report_TBL
group by resource;

select * from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL limit 10;

CREATE TABLE hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL1 as 
select *, 
case 
    when age < 18 then "UNDER 18"
    When age >=18 and age <=40 then "18-40"
    when age>=41 and age<=65 then "41-65"
    when age>65 then "OVER 65"
    else "UNKNOWN"
end as age
from hc_data_integration_cws.Master_Enrollment_Wide_Report_TBL ;

select * from abv_psp_amb_interaction_hum_mktg.hc_ambassador_interaction_master_tbl where activity_date='2012-03-26'

select abbvie_indiv_id,  Resource,  SubResource, promo_rsp_id, channel,  min(Enrollment_date) as Enrollment_date
    from hc_data_integration_cws.hc_ambassador_activity_refined
    group by abbvie_indiv_id,  Resource,  SubResource, promo_rsp_id, channel
    having min(enrollment_date)='2012-03-26'
    
    select abbvie_indiv_id, resource, subresource, channel, Enrollment_date from hc_data_integration_cws.1_Master_Enrollment
    where enrollment_date='2012-03-26'
group by resource, subresource, channel, Enrollment_date
 
 count(*) as REC_CNT, Count(distinct abbvie_indiv_id) as PAT_CNT

select * from hc_data_integration_cws.Master_Enrollment_Temp_1 where abbvie_indiv_id='53535333'

--Enrollment
select * from (select f.abbvie_indiv_id,'RELATIONSHIP MARKETING' as Resource, 
    upper(channel_dsc) as SubResource,'CRM99999' as promo_rsp_id, 'CRM99999' as channel, min(extract_date) as Enrollment_date
    from (select a.abbvie_indiv_id, channel_dsc, extract_date, RANK() over 
        (PARTITION BY a.abbvie_indiv_id order by extract_date asc) AS RowNo  
        from epsilon_psp_hum_mktg.eps_crm_hum_vw a
        LEFT JOIN epsilon_psp_hum_mktg.eps_base_indiv_hum_vw b
             on a.abbvie_indiv_id = b.abbvie_indiv_id
        where channel_dsc in ('Email','Direct Mail')  and  marketing_objective = 'Retention'
        and kit_cd not IN ('MYH_MEDREM_EM_C', 'HC_MEDREM_EM_A', 'HC_MEDREM_EM_B', 'HUM_SMS_RMND_A', 'HUM_SMS_DBL_A', 
        'HUM_IVR_RMND_A', 'MYH_MEDREM_EM2', 'MYHUM_HUGO_KIT', 'HUMCOM_HUGO_FT', 'SHRPS_KIT_A', 'SHRPS_KIT_B', 
        'SHRPS_KIT_C', 'SHRPS_KIT_D', 'SHRPS_KIT_E', 'SHRPS_KIT_F', 'SHRPS_KIT_G', 'SHRPS_KIT_H', 'MYHUM_ITK_KIT', 
        'HUMCOM_ITK_FT', 'PAPP_RMNDR')
        ) F WHERE F.RowNo = 1 
              GROUP BY f.abbvie_indiv_id, UPPER(channel_dsc)) x
              where abbvie_indiv_id='53535333'
              
--Fulfillment
select * from (
select abbvie_indiv_id,'Relationship Marketing' as Resource, channel_dsc as Tactic, b.kit_type as SubTactic, 
    case    when dlvry_stat_dsc = 'Fulfilled' and engager = 'Y' then 'Fulfilled - Engaged'
            when dlvry_stat_dsc = 'Fulfilled' and engager = 'N' then 'Fulfilled - Non Engaged'
        else dlvry_stat_dsc end as SubSubTactic,b.kit_nm as Activity,
    'CRM99999' as promo_rsp_id, 
    'CRM' as vehicle_dsc, 'CRM' as channel_dsc, 
    Null as engagement_interaction,
    extract_date as fullfillment_date
	from epsilon_psp_hum_mktg.eps_crm_hum_vw a
	left join hc_data_integration_cws.crm_mapping_kit_nm b 
	on a.kit_cd = b.kit_cd
	where marketing_objective like '%Retention%' AND channel_dsc NOT like '%Mobile%' AND channel_dsc NOT like '%Phone%'
	AND UPPER(dlvry_stat_dsc) Not IN ('CANNOT BE FULFILLED', 'DUPLICATE RECORD', 'INVALID ADDRESS') AND UPPER(channel_dsc) IN ('EMAIL', 'DIRECT MAIL')
	and a.kit_cd not IN ('MYH_MEDREM_EM_C', 'HC_MEDREM_EM_A', 'HC_MEDREM_EM_B', 'HUM_SMS_RMND_A', 'HUM_SMS_DBL_A', 
        'HUM_IVR_RMND_A', 'MYH_MEDREM_EM2', 'MYHUM_HUGO_KIT', 'HUMCOM_HUGO_FT', 'SHRPS_KIT_A', 'SHRPS_KIT_B', 
        'SHRPS_KIT_C', 'SHRPS_KIT_D', 'SHRPS_KIT_E', 'SHRPS_KIT_F', 'SHRPS_KIT_G', 'SHRPS_KIT_H', 'MYHUM_ITK_KIT', 
        'HUMCOM_ITK_FT', 'PAPP_RMNDR') and UPPER(dlvry_stat_dsc) <> 'COMPLETE')x where abbvie_indiv_id='53535333'
        
        
  select resource, subresource, channel, year(enrollment_date), month(enrollment_date), count(distinct abbvie_indiv_id) FROM ( 
   select
        abbvie_indiv_id, 
        case 
            when upper(x.Resource) in ('','NULL') or upper(x.Resource) is NULL then 'UNKNOWN'
            ELSE x.Resource
        end as Resource,
        case 
            when upper(x.SubResource) in ('','NULL') or upper(x.SubResource) is NULL then 'UNKNOWN'
            ELSE x.SubResource
        end as SubResource,
        case 
            when upper(x.channel) in ('','NULL') or upper(x.channel) is NULL then 'UNKNOWN'
            ELSE x.channel
        end as channel,
        Enrollment_date
    from 
    (
        select abbvie_indiv_id, Resource, SubResource, channel, Enrollment_date, RANK() over 
            (PARTITION BY abbvie_indiv_id, Resource, SubResource  order by channel asc) AS RowNo  
        from 
        (select abbvie_indiv_id, Resource, SubResource, channel,promo_rsp_id, min(Enrollment_date) as Enrollment_date
        from hc_data_integration_cws.Master_Enrollment_temp 
        where SubResource NOT IN ('MED REM APP', 'AMBASSADOR DISTRIBUTED MATERIALS')
        group by abbvie_indiv_id, Resource, SubResource, channel,promo_rsp_id) Y
    ) X
    where X.RowNo = 1
    order by abbvie_indiv_id) z
group by resource, subresource, channel, year(enrollment_date), month(enrollment_date)


--ENROLLMENT TEST QUERY
select distinct resource from hc_data_integration_cws.1_master_enrollment
    select resource, year(enr_dt), month(enr_dt), count(*) from (
    select Resource, abbvie_indiv_id, min(enrollment_date) as enr_dt from
    (
    select * from hc_data_integration_cws.1_master_enrollment
    ) y 
    group by
    resource, abbvie_indiv_id
    ) v
    group by resource, year(enr_dt), month(enr_dt)
    
    
    select distinct resource from hc_data_integration_cws.Master_Enrollment_Temp_1