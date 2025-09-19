FUNCTION-POOL ZSENDPAYSLIP.                 "MESSAGE-ID ..

  TYPES: BEGIN OF tp_ctrl,
           sender TYPE xubname,
           subject TYPE so_obj_des,
           spool TYPE rspoid,
           attach_as_msg TYPE c,
           attach_type TYPE so_obj_tp,
           subrc LIKE sy-subrc,
         END OF tp_ctrl.

  CONSTANTS: k_type_ali TYPE so_obj_tp VALUE 'ALI',
             k_type_otf TYPE so_obj_tp VALUE 'OTF',
             k_type_pdf TYPE so_obj_tp VALUE 'PDF',
             k_type_raw TYPE so_obj_tp VALUE 'RAW'.

  CONSTANTS: k_no_such_job          TYPE n VALUE 1,
             k_job_contains_no_data TYPE n VALUE 2,
             k_selection_empty      TYPE n VALUE 3,
             k_no_permission        TYPE n VALUE 4,
             k_can_not_access       TYPE n VALUE 5,
             k_read_error           TYPE n VALUE 6,
             k_type_no_match        TYPE n VALUE 7.

  CONSTANTS: k_too_many_receivers       TYPE n VALUE 1,
             k_document_not_sent          TYPE n VALUE 2,
             k_document_type_not_exist    TYPE n VALUE 3,
             k_operation_no_authorization TYPE n VALUE 4,
             k_parameter_error            TYPE n VALUE 5,
             k_x_error                    TYPE n VALUE 6,
             k_enqueue_error              TYPE n VALUE 7.
