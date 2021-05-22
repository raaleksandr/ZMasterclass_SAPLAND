@AbapCatalog.sqlViewName: 'ZDEPARTMENTS_XXX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
//@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Departments CDS View'

//BOPF
@ObjectModel:{
    compositionRoot: true,
    modelCategory: #BUSINESS_OBJECT,
    writeActivePersistence: 'zdepartments',
    transactionalProcessingEnabled: true,
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true
}

@OData.publish: true

define view ZDEPARTMENTS_CDS as select from zdepartments {
    key department_id,
    department_name,
    department_leader,
    department_address,
    department_room
}
