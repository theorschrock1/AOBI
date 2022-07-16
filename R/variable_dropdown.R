standard_dropdown_options=function(){
 tagList(dropdown_group(
    dropdown_button(inputId='add_to_sheet',label='Add to Sheet'),
    dropdown_button(inputId='show_filter',label='Show Filter')
  ),
  dropdown_group(
    dropdown_button(inputId='rename',label='Rename'),
    dropdown_button(inputId='edit',label='Edit...'),
    dropdown_button(inputId='duplicate',label='Duplicate'),
    dropdown_button(inputId='delete',label='Delete')
    ),
  dropdown_submenu(label="Create",
    dropdown_group(
      dropdown_button(inputId='create_calc_field',label='Calculated Field...'),
      dropdown_button(inputId='create_group',label='Group...'),
      dropdown_button(inputId='create_bins',label='Bins...'),
      dropdown_button(inputId='create_param',label='Parameter...')
                   )))
}
dropdrop_aggregation=function(){
  dropdown_submenu(label='Aggregation',
  dropdown_radio_group(inputId = 'aggregation',
                       options = toupper(c('sum',
                                           'min',
                                           'max',
                                           'mean',
                                           'median',
                                           'var',
                                           'sd',
                                           'n',
                                           'n_distinct')),
                         labels = toupper(c('sum',
                                           'min',
                                           'max',
                                           'mean',
                                           'median',
                                           'variance',
                                           'Std. Deviation',
                                           'Count',
                                           'Count distinct'))

                         )
  )
}
folders_dropdown=function(){
  dropdown_submenu(label='Folders',
    dropdown_button(inputId='add_to_folder',label='Add to Folder...'),
    dropdown_button(inputId='create_folder',label='Create Folder...')
  )
}
default_properties=function(){

    dropdown_submenu(label='Default Properties',
                     dropdrop_aggregation(),
                     dropdown_button(inputId='color_props',label='Color...'),
                     dropdown_button(inputId='number_format',label='Number Format...'),
                     dropdown_button(inputId='date_format',label='Date Format...'),
                     dropdown_button(inputId='label_format',label='Label Format...'),
                     dropdown_button(inputId='shape_props',label='Shape...'),
                     dropdown_button(inputId='order_props',label='Order...')
  )
}
variable_dropdown=function(id='variable_dropdown'){
data_dropdown(standard_dropdown_options(),
              default_properties(),
              folders_dropdown(),
              inputId=id,
              handle_class = 'dropdown-handle',
              target='.side-bar .pill-item')
}
