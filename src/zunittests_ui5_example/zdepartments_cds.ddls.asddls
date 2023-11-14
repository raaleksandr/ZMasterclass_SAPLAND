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

@UI.headerInfo: {
    typeName: 'Departments view',
    typeNamePlural: 'Departments View',
    title:  { type: #STANDARD, value: '_Customer.CompanyName' },
    description: { type: #STANDARD, value: '_Customer.CityName' }    
}

define view ZDEPARTMENTS_CDS as select from zdepartments {
    @UI.facet: [ { id:'idGeneralInformation' ,
                           type: #COLLECTION ,
                           label: 'General Information' ,
                           position: 10 } ,
                         { type: #IDENTIFICATION_REFERENCE ,
                           label : 'General Information',
                           parentId: 'idGeneralInformation' ,
                           id: 'idIdentification' } ]
    @Search: {
        defaultSearchElement: true,
        ranking: #MEDIUM,
        fuzzinessThreshold: 0.8
    }
    @UI: {
    	lineItem.position: 30,
    	selectionField.position: 30,
    	identification.position: 30
    }
    @EndUserText.label: 'Department ID'
    key department_id,
    @Search: {
        defaultSearchElement: true,
        ranking: #HIGH,
        fuzzinessThreshold: 0.8
    }
    @UI: {
        lineItem.position: 20,
        selectionField.position: 20,
        identification.position: 20
    }
    @EndUserText.label: 'Department name'
    department_name,
    @Search: {
        defaultSearchElement: true,
        ranking: #HIGH,
        fuzzinessThreshold: 0.8
    }
    @UI: {
        lineItem.position: 10,
        selectionField.position: 10,
        identification.position: 10
    }
    @EndUserText.label: 'Department leader'
    department_leader,
    @UI: {
        selectionField.position: 50,
        identification.position: 50
    }
    @EndUserText.label: 'Department address'
    department_address,
    @UI: {
        selectionField.position: 50,
        identification.position: 50
    }
    @EndUserText.label: 'Department room'
    department_room
}
