#' Renders the main menu drop-downs (FILE,EDIT,DATA,etc.) at the top of the application

#' @noRd
main_menu_dropdowns_ui=function(){
  navbar_row(class='navbar-dark bg-dark', main_menu_bar(
    # File ----
    dropdown_menu(
      label = "File",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    ),
    # Data ----
    dropdown_menu(
      label = "Data",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    ),
    # Worksheet ----
    dropdown_menu(
      label = "Worksheet",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    ),
    # Dashboard -----
    dropdown_menu(
      label = "Dashboard",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    ),

    # Analysis -----
    dropdown_menu(
      label = "Analysis",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    ),
    # View -----
    dropdown_menu(
      label = "View",
      class = 'menu-item',
      dropdown_btn_group(
        inputIds = letters[1:3],
        labels = c("Add to Sheet", "New Calculation", "Show Filter"),
        types = c("action", "action", "checkbox")
      ),
      dropdown_btn_group(
        inputIds = letters[4:6],
        labels = c("Copy", "Duplicate", "Edit"),
        types = c("action", "action", "model")
      ),
      dropdown_radio_group(
        inputId = "letters",
        options = letters[1:4],
        labels = c("Currency", "Percentage", "Number", "Units")
      ),
      dropdown_submenu(
        label = "More Options",
        dropdown_btn_group(
          inputIds = letters[1:2],
          labels = letters[1:2],
          types = c("action", "action")
        )
      )
    )

  ))
}

