with    cur_ay_year as (select 2021 as ACYR from DUAL)

      , prev_ay_year as (select 2020 as ACYR from DUAL)
-- Build parameter for the terms in the academic year, Winter & Spring of ACYR, Summer & Fall prior to ACYR
      , fw_registration_table.terms as (select    term_code                                        termcode
                           from    ban_termcodes
                          where    term_acyr_code in (select ACYR from cur_ay_year)
                            and    (SUBSTR(term_desc, 1, 4) = 'Fall'
                             or    SUBSTR(term_desc, 1, 6) = 'Winter'
                             or    SUBSTR(term_desc, 1, 6) = 'Spring')
                          union
                         select    term_code                                        termcode
                           from    ban_termcodes
                          where    term_acyr_code in (select ACYR - 1 from cur_ay_year)
                            and    SUBSTR(term_desc, 1, 6) = 'Summer'
                          union
                         select    term_code                                        termcode
                           from    ban_termcodes
                          where    term_acyr_code in (select ACYR from prev_ay_year)
                            and    (SUBSTR(term_desc, 1, 4) = 'Fall'
                             or    SUBSTR(term_desc, 1, 6) = 'Winter'
                             or    SUBSTR(term_desc, 1, 6) = 'Spring')
                          union
                         select    term_code                                        termcode
                           from    ban_termcodes
                          where    term_acyr_code in (select ACYR - 1 from prev_ay_year)
                            and    SUBSTR(term_desc, 1, 6) = 'Summer')

      , cur_term as   (select    MAX(term_code) as current_term
                         from    ban_termcodes
                        where    (select TRUNC(sysdate) from dual) > term_start_date
                          and    term_start_date < (select TRUNC(sysdate) from dual))
      , nxt_term as (select    termxwalk_term as next_term
                       from    termxwalk
                            , (select    termxwalk_num + 1 as nxt_term_walk
                                 from    termxwalk
                                where    termxwalk_term = (select current_term from cur_term)) nxt 
                      where  nxt.nxt_term_walk = termxwalk_num)


select    distinct stu_pidm                                             pidm
        , stu_id                                                        id
        , nvl(stu_preferred_first_name , stu_first)||' '||stu_last      student
        , recent.program                                                major
        , nvl(fw_1.term_desc,'Not Registered')                          fieldwork_I
        , nvl(fw_2.term_desc,'Not Registered')                          fieldwork_II
        , nvl(fw_3.term_desc,'Not Registered')                          fieldwork_III                 
        , recent.partnership                                            partnership
        , recent.campus                                                 campus
        , recent.coach                                                  advisor_coach 
        , case when  recent.campus = 'WH'
               then  'Bushra Sarkar'

                 when  recent.campus NOT in  ('WH', 'NT', 'TA')
                  and  recent.program in (  'BA ECP'
                                          , 'BA ECE'
                                          , 'BA SS/ECE'
                                          , 'BA LAS/ECE'
                                          , 'BA ELED'
                                          , 'BA SS/ELED'
                                          , 'BA LAS/ELED'
                                          , 'BA SPE'
                                          , 'BA SS/SPE'
                                          , 'BA LAS/SPE'
                                          , 'BA_ECED'
                                          , 'BA GSE'
                                          , '00 PBECE'
                                          , 'ND_ECED_CERT')
                 then  'Dee Williams'

                 when   recent.campus NOT in ('WH', 'TA')
                  and   recent.program in (   'BA BA'
                                            , 'BS CIS'
                                            , 'BA COMM'
                                            , 'BA AC'
                                            , 'BA_BUAD'
                                            , 'BA_BUAD_ONL'
                                            , 'BA_BUAD_PD')
                 then   'Matt Vicars'

                 when   recent.campus NOT in ('WH', 'TA')
                  and   recent.program in (   'BA CJ'
                                            , 'BA PSYCH'
                                            , 'BA HMS'
                                            , 'BA SOCSCI')
                 theN   'Jeanne McGill'

                 when  recent.program in (  'AAS_CULA'
                                          , 'BA_HOSM'
                                          , 'BA_CULA'
                                          , 'AAS_CULAACL'
                                          , 'AAS_BAPA'
                                          , 'CRTU PC'
                                          , 'CRTU BAPA'
                                          , 'BA_CULA_PD'
                                          , 'BA_HOSM_PD')
                 theN  'Deb Bosco'
                 else  'Unassigned'
                  end  career_advisor
  from    student_table
        , registration_table
        , (select     enrollment_table.pidm                                          pidm
                    , rec_term.termcode                                 termcode
                    , enrollment_table.program_code                                  program
                    , enrollment_table.partnership_type                              partnership
                    , enrollment_table.campus_code                                   campus 
                    , enrollment_table.category_expand_desc                          category
                    , enrollment_table.advisor                                       coach
                    , enrollment_table.level_code                                    deg_level  
             from    enrollment_table
                  , (  select    distinct enrollment_table.pidm                      pidm
                               , max(enrollment_table.term_code)                     termcode
                         from    enrollment
                     group by   enrollment_table.pidm) rec_term
            where    enrollment_table.pidm = rec_term.pidm
              and    enrollment_table.term_code = rec_term.termcode) recent 
        , (select    distinct registration_table.pidm as pidm
                   , case when registration_table.crse_status_code in ('W', 'WW', 'EW')
                          then registration_table.term_desc||' '||registration_table.crse_status_desc
                          else registration_table.term_desc
                      end  term_desc
             from    registration_table
                   , enrollment_table
            where    enrollment_table.pidm = registration_table.pidm
              and    registration_table.crse_subj_code||' '||registration_table.crse_numb = 'HSM 291'
             ) fw_1
        , (select    distinct registration_table.pidm as pidm
                   , case when registration_table.crse_status_code in ('W', 'WW', 'EW')
                          then registration_table.term_desc||' '||registration_table.crse_status_desc
                          else registration_table.term_desc
                      end  term_desc
             from    registration_table
                   , enrollment_table
            where    enrollment_table.pidm = registration_table.pidm
              and    registration_table.crse_subj_code||' '||registration_table.crse_numb = 'HSM 292'
             ) fw_2
        , (select    distinct registration_table.pidm as pidm
                   , case when registration_table.crse_status_code in ('W', 'WW', 'EW')
                          then registration_table.term_desc||' '||registration_table.crse_status_desc
                          else registration_table.term_desc
                      end  term_desc
             from    registration_table
                   , enrollment_table
            where    enrollment_table.pidm = registration_table.pidm
              and    registration_table.crse_subj_code||' '||registration_table.crse_numb = 'HSM 493'
             ) fw_3
 where    stu_pidm = registration_table.pidm
   and    registration_table.pidm = recent.pidm
   and    recent.pidm = fw_1.pidm (+)
   and    recent.pidm = fw_2.pidm (+)
   and    recent.pidm = fw_3.pidm (+)
   and    recent.partnership = 'Pathways'
   and    recent.program in ('BA HMS')
   and    f_pathways_year(stu_pidm, (select ACYR-1||90 from cur_ay_year)) not in ('Y1','Y2')
   and    registration_table.term_code in (select termcode from fw_registration_table.terms)
