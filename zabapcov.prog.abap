REPORT zabapcov.

* See https://github.com/larshp/abapCov/

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2016 Lars Hvam Petersen
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

PARAMETERS: p_prog   TYPE programm OBLIGATORY,
            p_file   TYPE text40 OBLIGATORY,
            p_token  TYPE text40 OBLIGATORY,
            p_commit TYPE text40 OBLIGATORY,
            p_branch TYPE text40 OBLIGATORY.

CLASS lcl_upload_codecov DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      upload
        IMPORTING it_meta TYPE cvt_stmnt_cov_meta_data.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_json
        IMPORTING it_meta        TYPE cvt_stmnt_cov_meta_data
        RETURNING VALUE(rv_json) TYPE string,
      call_url
        IMPORTING iv_json TYPE string.

ENDCLASS.

CLASS lcl_upload_codecov IMPLEMENTATION.

  METHOD upload.
    DATA(lv_json) = build_json( it_meta ).
    call_url( lv_json ).
  ENDMETHOD.

  METHOD call_url.

    DATA: li_client TYPE REF TO if_http_client,
          lv_code   TYPE i,
          lv_url    TYPE string.


    lv_url = 'https://codecov.io/upload/v2?token=' &&
      p_token &&
      '&commit=' &&
      p_commit &&
      '&branch=' &&
      p_branch ##NO_TEXT.

    cl_http_client=>create_by_url(
      EXPORTING
        url    = lv_url
        ssl_id = 'ANONYM'
      IMPORTING
        client = li_client ).

    li_client->request->set_cdata( iv_json ).

    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    li_client->send( ).
    li_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

    li_client->response->get_status(
      IMPORTING
        code   = lv_code ).

    DATA(lv_resp) = li_client->response->get_cdata( ).

    li_client->close( ).

    WRITE: / lv_code.
    WRITE: / lv_resp.

  ENDMETHOD.

  METHOD build_json.

    CONSTANTS: c_null TYPE c LENGTH 4 VALUE 'null' ##NO_TEXT.

    DATA: lt_lines TYPE TABLE OF string,
          lv_cov   TYPE string,
          lv_max   TYPE i.


    LOOP AT it_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      IF <ls_meta>-row > lv_max.
        lv_max = <ls_meta>-row.
      ENDIF.
    ENDLOOP.

    APPEND c_null TO lt_lines.
    DO lv_max TIMES.
      READ TABLE it_meta ASSIGNING <ls_meta> WITH KEY row = sy-tabix.
* 102 = covered
* 101 = uncovered
      IF sy-subrc = 0 AND <ls_meta>-color = 102.
        APPEND '1' TO lt_lines.
      ELSEIF sy-subrc = 0 AND <ls_meta>-color = 101.
        APPEND '0' TO lt_lines.
      ELSE.
        APPEND c_null TO lt_lines.
      ENDIF.
    ENDDO.

    CONCATENATE LINES OF lt_lines INTO lv_cov SEPARATED BY ','.
    rv_json = '{ "coverage": { "' &&
      p_file &&
      '": [' &&
      lv_cov &&
      '] } }' ##NO_TEXT.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      initialization,
      walk
        IMPORTING ii_node TYPE REF TO if_scv_result_node,
      node
        IMPORTING ii_node TYPE REF TO if_scv_result_node.

    CLASS-DATA:
      gt_source TYPE svt_src,
      gt_meta   TYPE cvt_stmnt_cov_meta_data,
      gi_result TYPE REF TO if_scv_result.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD initialization.

    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-cprog
        variant              = 'DEFAULT'
      EXCEPTIONS
        variant_not_existent = 01
        variant_obsolete     = 02 ##fm_subrc_ok. "#EC CI_SUBRC

  ENDMETHOD.

  METHOD run.

    DATA: lo_passport TYPE REF TO object.

    CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
      RECEIVING
        result = lo_passport.

    DATA(lo_runner) = cl_aucv_test_runner_coverage=>create( lo_passport ).

    DATA(lt_keys) = VALUE sabp_t_tadir_keys( ( obj_name = p_prog obj_type = 'PROG' ) ).

    lo_runner->run_for_program_keys(
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = lt_keys
      IMPORTING
        e_coverage_result            = DATA(li_coverage)
        e_aunit_result               = DATA(li_aunit) ).

    TRY.
        gi_result = li_coverage->build_coverage_result( ).
      CATCH cx_scv_execution_error cx_scv_call_error.
        BREAK-POINT.
    ENDTRY.

    LOOP AT gi_result->get_root_node( )->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

    DELETE gt_meta WHERE color = '30'.
    SORT gt_meta BY row ASCENDING.

    lcl_upload_codecov=>upload( gt_meta ).

  ENDMETHOD.

  METHOD walk.

    IF ii_node->has_children( ) = abap_false.
      node( ii_node ).
    ENDIF.

    LOOP AT ii_node->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

  ENDMETHOD.

  METHOD node.

    DATA: lv_pb_type    TYPE cvd_pb_type,
          lv_pb_name    TYPE cvd_pb_name,
          lv_prog_class TYPE cvd_prog_class,
          lv_prog_type  TYPE cvd_prog_type,
          lv_prog_name  TYPE cvd_prog_name.


    CASE ii_node->subtype.
      WHEN 'METH'.
        DATA(lo_insp) = cl_scv_pblock_inspector=>create( ii_node ).
        lv_pb_type    = 'METH'.
        lv_pb_name    = lo_insp->get_method_name( ).
        lv_prog_class = lo_insp->get_class_name( ).
        lv_prog_type  = lo_insp->get_program_subtype( ).
        lv_prog_name  = lo_insp->get_program_name( ).
      WHEN OTHERS.
        lv_pb_type   = ii_node->subtype.
        lv_pb_name   = ii_node->name.
        CLEAR lv_prog_class.
        lv_prog_type = ii_node->get_parent( )->subtype.
        lv_prog_name = ii_node->get_parent( )->name.
    ENDCASE.

    DATA(lo_ui_factory) = NEW cl_scov_stmnt_cov_ui_factory( ).

    DATA(lo_pb_info) = lo_ui_factory->create_pb_info(
      im_pb_type    = lv_pb_type
      im_pb_name    = lv_pb_name
      im_prog_class = lv_prog_class
      im_prog_type  = lv_prog_type
      im_prog_name  = lv_prog_name ).

    DATA(lt_tkey_selops) = VALUE cvt_test_key_selops( (
      option = 'EQ'
      sign   = 'I'
      low    = gi_result->get_measurement( )->get_testkey( ) ) ).

    DATA(li_container) = lo_ui_factory->create_stmnt_dcon_factory( lt_tkey_selops
      )->create_stmnt_data_container( lo_pb_info ).

    IF gt_source IS INITIAL.
      gt_source = li_container->get_source( ).
    ELSEIF li_container->get_source( ) <> gt_source.
* ignore includes, todo
      RETURN.
    ENDIF.

    DATA(lt_meta) = li_container->get_stmnt_cov_meta_data( ).
    APPEND LINES OF lt_meta TO gt_meta.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).

INITIALIZATION.
  lcl_app=>initialization( ).