sheet_aggregation=function(){
  dropdown_submenu(label='Aggregation',
                   dropdown_radio_group(inputId = 'aggregation',
                                        trigger_event = 'rename',
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

sheet_dropdown=function(){
data_dropdown(
  sheet_aggregation(),
  inputId='sheet_dropdown',
  handle_class = 'dropdown-handle',
  target='.ao-sheet .pill-item')

}
