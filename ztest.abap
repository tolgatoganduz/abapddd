REPORT ztest.

TYPES ty_account_id TYPE saknr.
TYPES ty_balance TYPE dmbtr.
TYPES ty_amount TYPE dmbtr.

CLASS lcl_account DEFINITION DEFERRED.
CLASS lcl_manager DEFINITION DEFERRED.

TYPES ty_account_t TYPE STANDARD TABLE OF REF TO lcl_account.

INTERFACE lfl_create_account.

  METHODS execute
    IMPORTING
              i_initial_balance TYPE ty_balance
    RETURNING VALUE(result)     TYPE ty_account_id.

ENDINTERFACE.

INTERFACE lfl_transfer_money.

  METHODS execute
    IMPORTING
              VALUE(i_from_account_id) TYPE ty_account_id
              VALUE(i_to_account_id)   TYPE ty_account_id
              VALUE(i_amount)          TYPE ty_amount
    RETURNING VALUE(result)            TYPE abap_bool.

ENDINTERFACE.

INTERFACE lfl_account_factory.

  METHODS create
    IMPORTING
              VALUE(i_initial_balance) TYPE ty_balance
    RETURNING VALUE(result)            TYPE REF TO lcl_account.

ENDINTERFACE.

INTERFACE lfl_account_repository.

  METHODS get_by_id
    IMPORTING
              VALUE(i_account_id) TYPE ty_account_id
    RETURNING VALUE(result)       TYPE REF TO lcl_account.

  METHODS save
    IMPORTING
              i_account     TYPE REF TO lcl_account OPTIONAL
              i_account_t   TYPE ty_account_t OPTIONAL
    RETURNING VALUE(result) TYPE abap_bool.

ENDINTERFACE.


CLASS lcl_account DEFINITION FINAL
    CREATE PRIVATE
    FRIENDS lfl_account_factory lfl_account_repository.

  PUBLIC SECTION.

    DATA id TYPE ty_account_id READ-ONLY.

    DATA balance TYPE ty_balance READ-ONLY.

    DATA closed TYPE abap_bool READ-ONLY.

    DATA negative_balance_allowed TYPE abap_bool VALUE abap_false READ-ONLY.

    METHODS deposit
      IMPORTING VALUE(i_amount) TYPE ty_amount
      RETURNING VALUE(result)   TYPE abap_bool.

    METHODS withdraw
      IMPORTING VALUE(i_amount) TYPE ty_amount
      RETURNING VALUE(result)   TYPE abap_bool.

    METHODS close
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS allow_negative_balance
      IMPORTING VALUE(i_allow) TYPE abap_bool
      RETURNING VALUE(result)  TYPE abap_bool.

ENDCLASS.


CLASS lcl_account IMPLEMENTATION.

  METHOD deposit.

    IF i_amount > 0 AND closed = abap_false.

      balance = balance + i_amount.

      result = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD withdraw.

    IF i_amount > 0 AND closed = abap_false.

      IF balance >= i_amount OR negative_balance_allowed = abap_true.

        balance = balance - i_amount.

        result = abap_true.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD close.

    IF closed = abap_true.

      result = abap_true.

    ELSEIF balance >= 0.

      closed = abap_true.

      result = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD allow_negative_balance.

    IF closed = abap_false.

      negative_balance_allowed = i_allow.

      result = abap_true.

    ENDIF.

  ENDMETHOD.

ENDCLASS.






CLASS lcl_account_factory DEFINITION FINAL CREATE PRIVATE
    FRIENDS lcl_manager.

  PUBLIC SECTION.

    INTERFACES lfl_account_factory.

  PRIVATE SECTION.

    TYPES ty_account_id_counter TYPE numc10.

    DATA account_id_counter TYPE ty_account_id_counter VALUE 0.

ENDCLASS.

CLASS lcl_account_factory IMPLEMENTATION.

  METHOD lfl_account_factory~create.

    IF i_initial_balance >= 0.

      ADD 1 TO account_id_counter.

      result = NEW #( ).

      result->id = account_id_counter.
      result->balance = i_initial_balance.
      result->negative_balance_allowed = abap_false.
      result->closed = abap_false.

    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_account_repository DEFINITION FINAL CREATE PRIVATE
    FRIENDS lcl_manager.

  PUBLIC SECTION.

    INTERFACES lfl_account_repository.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_map,

             account_id TYPE ty_account_id,

             account    TYPE REF TO lcl_account,

           END OF ty_map.

    TYPES ty_map_t TYPE HASHED TABLE OF ty_map WITH UNIQUE KEY account_id.

    DATA map_t TYPE ty_map_t.

ENDCLASS.

CLASS lcl_account_repository IMPLEMENTATION.

  METHOD lfl_account_repository~get_by_id.

    ASSIGN map_t[ account_id = i_account_id ] TO FIELD-SYMBOL(<lf_map>).

    IF sy-subrc = 0.

      result = <lf_map>-account.

    ENDIF.

  ENDMETHOD.


  METHOD lfl_account_repository~save.

    " store account object to db

    DATA lt_account TYPE ty_account_t.

    REFRESH lt_account.

    APPEND LINES OF i_account_t TO lt_account.

    IF i_account IS NOT INITIAL.

      APPEND i_account TO lt_account.

    ENDIF.

    DELETE lt_account WHERE table_line IS INITIAL.

    LOOP AT lt_account ASSIGNING FIELD-SYMBOL(<lf_account>).

      ASSIGN map_t[ account_id = <lf_account>->id ] TO FIELD-SYMBOL(<lf_map>).

      IF sy-subrc = 0.

        <lf_map>-account = <lf_account>.

      ELSE.

        DATA(ls_map) = VALUE ty_map( account_id = <lf_account>->id account = <lf_account> ).

        INSERT ls_map INTO TABLE map_t.

      ENDIF.

    ENDLOOP.

    result = abap_true.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_transfer_money DEFINITION FINAL CREATE PRIVATE
    FRIENDS lcl_manager.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_repository TYPE REF TO lfl_account_repository.

    INTERFACES lfl_transfer_money.


  PRIVATE SECTION.

    DATA repository TYPE REF TO lfl_account_repository.



ENDCLASS.


CLASS lcl_transfer_money IMPLEMENTATION.

  METHOD constructor.

    repository = i_repository.

  ENDMETHOD.

  METHOD lfl_transfer_money~execute.

    DATA(lo_from_account) = repository->get_by_id( i_from_account_id ).

    DATA(lo_to_account) = repository->get_by_id( i_to_account_id ).


    IF lo_from_account IS NOT INITIAL AND lo_to_account IS NOT INITIAL.

      IF lo_from_account->withdraw( i_amount = i_amount ) = abap_true.

        IF lo_to_account->deposit( i_amount = i_amount ) = abap_true.

          DATA lt_account TYPE ty_account_t.

          REFRESH lt_account.

          APPEND lo_from_account TO lt_account.

          APPEND lo_to_account TO lt_account.

          IF repository->save( i_account_t = lt_account ) = abap_true.

            COMMIT WORK.

            result = abap_true.

          ELSE.

            ROLLBACK WORK.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_create_account DEFINITION FINAL CREATE PRIVATE
    FRIENDS lcl_manager.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        i_factory    TYPE REF TO lfl_account_factory
        i_repository TYPE REF TO lfl_account_repository.


    INTERFACES lfl_create_account.


  PRIVATE SECTION.

    DATA factory TYPE REF TO lfl_account_factory.

    DATA repository TYPE REF TO lfl_account_repository.

ENDCLASS.

CLASS lcl_create_account IMPLEMENTATION.

  METHOD constructor.

    factory = i_factory.

    repository = i_repository.

  ENDMETHOD.

  METHOD lfl_create_account~execute.

    DATA(lo_account) = factory->create( i_initial_balance = i_initial_balance ).

    IF lo_account IS NOT INITIAL.

      IF repository->save( i_account = lo_account ) = abap_true.

        COMMIT WORK.

        result = lo_account->id.

      ELSE.

        ROLLBACK WORK.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_manager DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS create_repository
      RETURNING VALUE(result) TYPE REF TO lfl_account_repository.

    CLASS-METHODS create_factory
      RETURNING VALUE(result) TYPE REF TO lfl_account_factory.

    CLASS-METHODS create_account_create_service
      RETURNING VALUE(result) TYPE REF TO lfl_create_account.

    CLASS-METHODS create_transfer_money_service
      RETURNING VALUE(result) TYPE REF TO lfl_transfer_money.


  PRIVATE SECTION.

    CLASS-DATA repository TYPE REF TO lfl_account_repository.

    CLASS-DATA factory TYPE REF TO lfl_account_factory.

ENDCLASS.

CLASS lcl_manager IMPLEMENTATION.

  METHOD create_repository.

    IF repository IS INITIAL.

      repository = NEW lcl_account_repository( ).

    ENDIF.

    result = repository.

  ENDMETHOD.


  METHOD create_factory.

    IF factory IS INITIAL.

      factory = NEW lcl_account_factory( ).

    ENDIF.

    result = factory.

  ENDMETHOD.

  METHOD create_account_create_service.

    result = NEW lcl_create_account(
      i_factory = create_factory( )
      i_repository = create_repository( ) ).

  ENDMETHOD.

  METHOD create_transfer_money_service.

    result = NEW lcl_transfer_money(
      i_repository = create_repository( ) ).

  ENDMETHOD.


ENDCLASS.

START-OF-SELECTION.

  PERFORM main.


FORM main.

  DATA(lo_create_service) = lcl_manager=>create_account_create_service( ).

  DATA(lo_transfer_service) = lcl_manager=>create_transfer_money_service( ).

  DATA(lv_from_id) = lo_create_service->execute( i_initial_balance = 350 ).

  DATA(lv_to_id) = lo_create_service->execute( i_initial_balance = 1000 ).

  IF lo_transfer_service->execute(
      i_from_account_id = lv_from_id
      i_to_account_id   = lv_to_id
      i_amount          = 400 ) = abap_false.

    DATA(lo_repository) = lcl_manager=>create_repository( ).

    DATA(lo_account) = lo_repository->get_by_id( i_account_id = lv_from_id ).

    IF lo_account IS NOT INITIAL.

      lo_account->allow_negative_balance( abap_true ).

      IF lo_repository->save( i_account = lo_account ) = abap_true.

        COMMIT WORK.

        IF lo_transfer_service->execute(
          i_from_account_id = lv_from_id
          i_to_account_id   = lv_to_id
          i_amount          = 400 ) = abap_true.

          MESSAGE 'Transfer OK' TYPE 'S'.

        ELSE.

          MESSAGE 'Transfer not OK' TYPE 'E'.

        ENDIF.

      ELSE.

        ROLLBACK WORK.

        MESSAGE 'Account not updates successfully' TYPE 'E'.

      ENDIF.

    ENDIF.

  ELSE.

    MESSAGE 'Transfer OK' TYPE 'S'.

  ENDIF.

ENDFORM.