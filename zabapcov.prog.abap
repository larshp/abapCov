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

TABLES: rs38m,
        seoclass.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_devc TYPE tadir-devclass OBLIGATORY.
SELECT-OPTIONS: s_prog FOR rs38m-programm,
                s_clas FOR seoclass-clsname.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_sprog TYPE text40 OBLIGATORY DEFAULT '.prog.abap' ##NO_TEXT,
            p_sclas TYPE text40 OBLIGATORY DEFAULT '.clas.abap' ##NO_TEXT.
PARAMETERS: p_lower TYPE c AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_token  TYPE text40 OBLIGATORY,
            p_commit TYPE text40 OBLIGATORY,
            p_branch TYPE text40 OBLIGATORY DEFAULT 'master' ##NO_TEXT.
SELECTION-SCREEN END OF BLOCK b3.


CLASS lcl_files DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      push
        IMPORTING
          is_key  TYPE sabp_s_tadir_key
          it_meta TYPE cvt_stmnt_cov_meta_data,
      get_count
        RETURNING VALUE(rv_count) TYPE i,
      pop
        EXPORTING
          ev_filename TYPE string
          et_meta     TYPE cvt_stmnt_cov_meta_data.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_file,
             key  TYPE sabp_s_tadir_key,
             meta TYPE cvt_stmnt_cov_meta_data,
           END OF ty_file.

    DATA: mt_files TYPE STANDARD TABLE OF ty_file.

ENDCLASS.

CLASS lcl_files IMPLEMENTATION.

  METHOD push.
    APPEND VALUE #( key = is_key meta = it_meta ) TO mt_files.
  ENDMETHOD.

  METHOD get_count.
    rv_count = lines( mt_files ).
  ENDMETHOD.

  METHOD pop.

    ev_filename = mt_files[ 1 ]-key-obj_name.
    IF p_lower = abap_true.
      TRANSLATE ev_filename TO LOWER CASE.
    ENDIF.

    CASE mt_files[ 1 ]-key-obj_type.
      WHEN 'PROG'.
        CONCATENATE ev_filename p_sprog INTO ev_filename.
      WHEN 'CLAS'.
        CONCATENATE ev_filename p_sclas INTO ev_filename.
    ENDCASE.

    et_meta = mt_files[ 1 ]-meta.

    DELETE mt_files INDEX 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_upload_codecov DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      upload
        IMPORTING io_files TYPE REF TO lcl_files.

  PRIVATE SECTION.
    CLASS-METHODS:
      build_array
        IMPORTING it_meta         TYPE cvt_stmnt_cov_meta_data
        RETURNING VALUE(rv_array) TYPE string,
      build_json
        IMPORTING io_files       TYPE REF TO lcl_files
        RETURNING VALUE(rv_json) TYPE string,
      call_url
        IMPORTING iv_json TYPE string.

ENDCLASS.

CLASS lcl_upload_codecov IMPLEMENTATION.

  METHOD upload.
    DATA(lv_json) = build_json( io_files ).
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
    WHILE strlen( lv_resp ) > 120.
      WRITE: / lv_resp(120).
      lv_resp = lv_resp+120.
    ENDWHILE.
    WRITE: / lv_resp.

  ENDMETHOD.

  METHOD build_array.

    CONSTANTS: lc_null TYPE c LENGTH 4 VALUE 'null' ##NO_TEXT.

    DATA: lt_lines TYPE TABLE OF string,
          lv_max   TYPE i.


    LOOP AT it_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      IF <ls_meta>-row > lv_max.
        lv_max = <ls_meta>-row.
      ENDIF.
    ENDLOOP.

    APPEND lc_null TO lt_lines.
    DO lv_max TIMES.
      READ TABLE it_meta ASSIGNING <ls_meta> WITH KEY row = sy-tabix.
* 102 = covered
* 101 = uncovered
      IF sy-subrc = 0 AND <ls_meta>-color = 102.
        APPEND '1' TO lt_lines.
      ELSEIF sy-subrc = 0 AND <ls_meta>-color = 101.
        APPEND '0' TO lt_lines.
      ELSE.
        APPEND lc_null TO lt_lines.
      ENDIF.
    ENDDO.

    CONCATENATE LINES OF lt_lines INTO rv_array SEPARATED BY ','.
    CONCATENATE '[' rv_array ']' INTO rv_array.

  ENDMETHOD.

  METHOD build_json.

    DATA: lt_files TYPE TABLE OF string.


    DO io_files->get_count( ) TIMES.
      io_files->pop(
        IMPORTING
          ev_filename = DATA(lv_filename)
          et_meta     = DATA(lt_meta) ).
      IF lines( lt_meta ) = 0.
        CONTINUE.
      ENDIF.
      DATA(lv_array) = build_array( lt_meta ).
      APPEND |"{ lv_filename }": { lv_array }| TO lt_files.
    ENDDO.

    CONCATENATE LINES OF lt_files INTO rv_json SEPARATED BY ','.

    rv_json = '{ "coverage": { ' &&
      rv_json &&
      ' } }' ##NO_TEXT.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_source DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      read IMPORTING is_tadir         TYPE sabp_s_tadir_key
           RETURNING VALUE(rt_source) TYPE svt_src.

  PRIVATE SECTION.
    CLASS-METHODS:
      clas IMPORTING iv_obj_name      TYPE tadir-obj_name
           RETURNING VALUE(rt_source) TYPE svt_src,
      prog IMPORTING iv_obj_name      TYPE tadir-obj_name
           RETURNING VALUE(rt_source) TYPE svt_src,
      remove_signatures
        CHANGING ct_source TYPE svt_src.

ENDCLASS.

CLASS lcl_source IMPLEMENTATION.

  METHOD read.

    CASE is_tadir-obj_type.
      WHEN 'PROG'.
        rt_source = prog( is_tadir-obj_name ).
      WHEN 'CLAS'.
        rt_source = clas( is_tadir-obj_name ).
    ENDCASE.

  ENDMETHOD.

  METHOD remove_signatures.

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE sap_bool.


    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO DATA(lv_source).
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD clas.

    DATA: ls_clskey TYPE seoclskey,
          lo_source TYPE REF TO cl_oo_source.


    ls_clskey-clsname = iv_obj_name.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = ls_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    ASSERT sy-subrc = 0.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).

    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.

  METHOD prog.
    READ REPORT iv_obj_name INTO rt_source.
    ASSERT sy-subrc = 0.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_mapper DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_tadir TYPE sabp_s_tadir_key,
      is_valid
        IMPORTING it_source       TYPE svt_src
        RETURNING VALUE(rv_valid) TYPE abap_bool,
      map
        IMPORTING it_source        TYPE svt_src
        RETURNING VALUE(rv_offset) TYPE i.

  PRIVATE SECTION.
    DATA: mt_source TYPE svt_src,
          ms_tadir  TYPE sabp_s_tadir_key.

ENDCLASS.

CLASS lcl_mapper IMPLEMENTATION.

  METHOD constructor.
    ms_tadir = is_tadir.
    mt_source = lcl_source=>read( ms_tadir ).
  ENDMETHOD.

  METHOD map.

    DATA: lv_match  TYPE abap_bool,
          lv_offset TYPE i.


    IF ms_tadir-obj_type = 'PROG'.
      RETURN.
    ENDIF.

    DO lines( mt_source ) TIMES.
      lv_offset = sy-index - 1.

      lv_match = abap_true.
      LOOP AT mt_source ASSIGNING FIELD-SYMBOL(<lv_source>)
          FROM lv_offset + 1 TO lv_offset + lines( it_source ).
        READ TABLE it_source INDEX sy-tabix - lv_offset ASSIGNING FIELD-SYMBOL(<lv_input>).
        ASSERT sy-subrc = 0.

        IF <lv_source> <> <lv_input>.
          lv_match = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_match = abap_true.
        rv_offset = lv_offset.
        RETURN.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD is_valid.

    CASE ms_tadir-obj_type.
      WHEN 'PROG'.
* todo, for PROG it currently filters out results from INCLUDES
        rv_valid = boolc( mt_source = it_source ).
      WHEN 'CLAS'.
        rv_valid = abap_true.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_runner DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_tadir TYPE sabp_s_tadir_key,
      run
        RETURNING VALUE(rt_meta) TYPE cvt_stmnt_cov_meta_data.

  PRIVATE SECTION.
    METHODS:
      get_runner
        RETURNING VALUE(ro_runner) TYPE REF TO cl_aucv_test_runner_coverage,
      walk
        IMPORTING ii_node TYPE REF TO if_scv_result_node,
      node
        IMPORTING ii_node TYPE REF TO if_scv_result_node.

    DATA:
      ms_tadir  TYPE sabp_s_tadir_key,
      mt_meta   TYPE cvt_stmnt_cov_meta_data,
      mo_mapper TYPE REF TO lcl_mapper,
      mi_result TYPE REF TO if_scv_result.

ENDCLASS.

CLASS lcl_runner IMPLEMENTATION.

  METHOD constructor.
    ms_tadir = is_tadir.
    mo_mapper = NEW lcl_mapper( ms_tadir ).
  ENDMETHOD.

  METHOD walk.

    IF ii_node->has_children( ) = abap_false.
      node( ii_node ).
    ENDIF.

    LOOP AT ii_node->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

  ENDMETHOD.

  METHOD get_runner.

    DATA: lo_passport TYPE REF TO object.


    CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
      RECEIVING
        result = lo_passport.

    ro_runner = cl_aucv_test_runner_coverage=>create( lo_passport ).

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
      low    = mi_result->get_measurement( )->get_testkey( ) ) ).

    DATA(li_container) = lo_ui_factory->create_stmnt_dcon_factory( lt_tkey_selops
      )->create_stmnt_data_container( lo_pb_info ).

    IF mo_mapper->is_valid( li_container->get_source( ) ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lv_offset) = mo_mapper->map( li_container->get_source( ) ).

    DATA(lt_meta) = li_container->get_stmnt_cov_meta_data( ).

    LOOP AT lt_meta ASSIGNING FIELD-SYMBOL(<ls_meta>).
      <ls_meta>-row = <ls_meta>-row + lv_offset.
    ENDLOOP.

    APPEND LINES OF lt_meta TO mt_meta.

  ENDMETHOD.

  METHOD run.

    get_runner( )->run_for_program_keys(
      EXPORTING
        i_limit_on_duration_category = '36' " long
        i_limit_on_risk_level        = '33' " critical
        i_program_keys               = VALUE #( ( CORRESPONDING #( ms_tadir ) ) )
      IMPORTING
        e_coverage_result            = DATA(li_coverage)
        e_aunit_result               = DATA(li_aunit) ).

    TRY.
        mi_result = li_coverage->build_coverage_result( ).
      CATCH cx_scv_execution_error cx_scv_call_error.
        ASSERT 0 = 1.
    ENDTRY.

    LOOP AT mi_result->get_root_node( )->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

    DELETE mt_meta WHERE color = '30'.
    SORT mt_meta BY row ASCENDING.

    rt_meta = mt_meta.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run,
      initialization.

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

    DATA: lv_category TYPE seoclassdf-category,
          lt_keys     TYPE sabp_t_tadir_keys.


    SELECT obj_name object FROM tadir
      INTO TABLE lt_keys
      WHERE devclass = p_devc
      AND object = 'PROG'
      AND obj_name IN s_prog.             "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT obj_name object FROM tadir
      APPENDING TABLE lt_keys
      WHERE devclass = p_devc
      AND object = 'CLAS'
      AND obj_name IN s_clas.             "#EC CI_SUBRC "#EC CI_GENBUFF

    DATA(lo_files) = NEW lcl_files( ).

    LOOP AT lt_keys ASSIGNING FIELD-SYMBOL(<ls_key>).
      IF <ls_key>-obj_type = 'CLAS'.
        SELECT SINGLE category FROM seoclassdf
          INTO lv_category
          WHERE clsname = <ls_key>-obj_name
          AND version = '1'.
        IF sy-subrc = 0 AND lv_category = '05'.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA(lt_meta) = NEW lcl_runner( <ls_key> )->run( ).
      lo_files->push( is_key  = <ls_key>
                      it_meta = lt_meta ).
    ENDLOOP.

    IF lo_files->get_count( ) > 0.
      lcl_upload_codecov=>upload( lo_files ).
    ELSE.
      WRITE: / 'No objects found'(004).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_app=>run( ).

INITIALIZATION.
  lcl_app=>initialization( ).