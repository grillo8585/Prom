CLASS zcl_bclog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_object    TYPE balobj_d OPTIONAL
        !i_subobject TYPE balsubobj OPTIONAL
        !i_prog      TYPE sycprog OPTIONAL
        !i_id        TYPE balnrext OPTIONAL .
    METHODS add_s_message_bapiret
      IMPORTING
        !is_single_message TYPE bapiret2 .
    METHODS add_t_message_bapiret
      IMPORTING
        !it_message TYPE bapiret2_t .
    METHODS show_log .
    METHODS refresh_log .
    METHODS save_db
      IMPORTING
        !iv_in_update_task TYPE xfeld .
    METHODS show_multiple_log
      IMPORTING
        !iv_date_from       TYPE datum
        !iv_date_to         TYPE datum
        !iv_show_sc         TYPE xfeld
        !is_display_profile TYPE bal_s_prof .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gv_object TYPE balobj_d .
    DATA gv_sobject TYPE balsubobj .
    DATA gv_repid TYPE sycprog .
    DATA gs_msg_default TYPE bal_s_mdef .
ENDCLASS.



CLASS ZCL_BCLOG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->ADD_S_MESSAGE_BAPIRET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SINGLE_MESSAGE              TYPE        BAPIRET2
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_s_message_bapiret.
    DATA: ls_msg TYPE bal_s_msg.

    ls_msg-msgid = is_single_message-id.
    ls_msg-msgty = is_single_message-type.
    ls_msg-msgno = is_single_message-number.
    ls_msg-msgv1 = is_single_message-message_v1.
    ls_msg-msgv2 = is_single_message-message_v2.
    ls_msg-msgv3 = is_single_message-message_v3.
    ls_msg-msgv4 = is_single_message-message_v4.

    CASE ls_msg-msgty.
      WHEN 'E'.
        ls_msg-probclass = 1.
      WHEN 'W'.
        ls_msg-probclass = 2.
      WHEN 'I'.
        ls_msg-probclass = 3.
      WHEN 'S'.
        ls_msg-probclass = 4.
    ENDCASE.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_s_msg       = ls_msg
        i_log_handle  = me->gs_msg_default-log_handle
      EXCEPTIONS
        log_not_found = 0
        OTHERS        = 1.
    IF sy-subrc EQ 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->ADD_T_MESSAGE_BAPIRET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MESSAGE                     TYPE        BAPIRET2_T
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD add_t_message_bapiret.
    FIELD-SYMBOLS <ls_return> TYPE bapiret2.

    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
      EXPORTING
        i_s_msg_defaults = me->gs_msg_default
      EXCEPTIONS
        OTHERS           = 0.

    LOOP AT it_message ASSIGNING <ls_return>.
      me->add_s_message_bapiret( <ls_return> ).
    ENDLOOP.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJECT                       TYPE        BALOBJ_D(optional)
* | [--->] I_SUBOBJECT                    TYPE        BALSUBOBJ(optional)
* | [--->] I_PROG                         TYPE        SYCPROG(optional)
* | [--->] I_ID                           TYPE        BALNREXT(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA: ls_log TYPE bal_s_log.  " Estructura para crear Log

    me->gv_object  = i_object.
    me->gv_sobject = i_subobject.
    IF i_prog IS INITIAL.
      me->gv_repid = sy-cprog. " Programa donde se realiza la instancia
    ELSE.
      me->gv_repid = i_prog.   " Programa donde se realiza la instancia
    ENDIF.

*--- Estructura LOG
    ls_log-extnumber = sy-title.     " Nombre programa
    ls_log-aluser    = sy-uname.     " Usuario genrador
    ls_log-object    = i_object.     " Objeto
    ls_log-subobject = i_subobject.  " Sub objeto
    ls_log-alprog    = me->gv_repid. " Programa donde se realiza la instancia
    ls_log-extnumber = i_id.         " ID Externo
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = ls_log
      IMPORTING
        e_log_handle            = me->gs_msg_default-log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->REFRESH_LOG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD refresh_log.
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = me->gs_msg_default-log_handle
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->SAVE_DB
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_IN_UPDATE_TASK              TYPE        XFELD
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD save_db.
    DATA lt_logh TYPE bal_t_logh.

    APPEND me->gs_msg_default-log_handle TO lt_logh.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_in_update_task = iv_in_update_task
        i_t_log_handle   = lt_logh
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->SHOW_LOG
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_log.
    DATA: l_s_display_profile TYPE bal_s_prof,
          wa_test             TYPE bal_s_sort,
          t_log_handle        TYPE bal_t_logh.

    APPEND me->gs_msg_default-log_handle TO t_log_handle.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = l_s_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid
      TYPE sy-msgty
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = l_s_display_profile
        i_t_log_handle      = t_log_handle
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid
      TYPE 'S'
      NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_BCLOG->SHOW_MULTIPLE_LOG
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DATE_FROM                   TYPE        DATUM
* | [--->] IV_DATE_TO                     TYPE        DATUM
* | [--->] IV_SHOW_SC                     TYPE        XFELD
* | [--->] IS_DISPLAY_PROFILE             TYPE        BAL_S_PROF
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD show_multiple_log.
    CALL FUNCTION 'APPL_LOG_DISPLAY'
      EXPORTING
        object                    = me->gv_object
        subobject                 = me->gv_sobject
        date_from                 = iv_date_from
        date_to                   = iv_date_to
        object_attribute          = 1
        subobject_attribute       = 1
        external_number_attribute = 0
        suppress_selection_dialog = iv_show_sc
        i_s_display_profile       = is_display_profile
      EXCEPTIONS
        no_authority              = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
