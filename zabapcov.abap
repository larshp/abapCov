REPORT zabapcov.

PARAMETERS: pv_prog TYPE programm OBLIGATORY.

*parameters:
*filename
*token
*commit
*branch
*job


* 102 = covered
* 101 = uncovered

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

    DATA(lt_keys) = VALUE sabp_t_tadir_keys( ( obj_name = pv_prog obj_type = 'PROG' ) ).

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
      CATCH cx_scv_execution_error cx_scv_call_error .
        BREAK-POINT.
    ENDTRY.

    LOOP AT gi_result->get_root_node( )->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

    DELETE gt_meta WHERE color = '30'.
    SORT gt_meta BY row ASCENDING.
    BREAK-POINT.

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
        DATA(mi) = cl_scv_pblock_inspector=>create( ii_node ).
        lv_pb_type    = 'METH'.
        lv_pb_name    = mi->get_method_name( ).
        lv_prog_class = mi->get_class_name( ).
        lv_prog_type  = mi->get_program_subtype( ).
        lv_prog_name  = mi->get_program_name( ).
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
