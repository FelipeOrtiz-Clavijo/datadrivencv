summary_box <- function(title, value, status = "primary") {
  colors <- c(
    primary = "#0d6efd",
    success = "#198754",
    info = "#0dcaf0",
    warning = "#ffc107",
    danger = "#dc3545",
    muted = "#6c757d"
  )
  color <- colors[[status]]
  if (is.null(color)) {
    color <- colors[["muted"]]
  }
  shiny::div(
    class = "summary-box",
    style = sprintf("border-left: 6px solid %s; padding: 1rem; background-color: #f8f9fa; border-radius: 8px; margin-bottom: 1rem;", color),
    shiny::h3(style = "margin: 0; font-weight: 600;", shiny::span(value, style = sprintf("color:%s", color))),
    shiny::p(style = "margin: 0.25rem 0 0; color: #495057;", title)
  )
}

format_meeting_status <- function(meetings) {
  if (!nrow(meetings)) {
    return(0L)
  }
  upcoming <- meetings$fecha >= Sys.Date()
  sum(upcoming, na.rm = TRUE)
}

empty_students <- function() {
  data.frame(
    id = integer(),
    nombre = character(),
    email = character(),
    programa = character(),
    fecha_inicio = as.Date(character()),
    estado = character(),
    stringsAsFactors = FALSE
  )
}

empty_projects <- function() {
  data.frame(
    id = integer(),
    titulo = character(),
    estudiante = character(),
    asesor = character(),
    estado = character(),
    fecha_entrega = as.Date(character()),
    descripcion = character(),
    stringsAsFactors = FALSE
  )
}

empty_meetings <- function() {
  data.frame(
    id = integer(),
    proyecto = character(),
    fecha = as.Date(character()),
    participantes = character(),
    acuerdos = character(),
    comentarios = character(),
    stringsAsFactors = FALSE
  )
}

next_id <- function(df) {
  if (!nrow(df)) {
    return(1L)
  }
  max(df$id, na.rm = TRUE) + 1L
}

sanitize_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(x)
}

ui <- shiny::navbarPage(
  title = "Dirección de Trabajos de Grado",
  header = shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        "body { background-color: #f4f6f9; }\n          .nav-tabs>li>a, .navbar-nav>li>a { font-weight: 500; }\n          .summary-box h3 { font-size: 2rem; }\n          .dataTables_wrapper .dataTables_filter input { margin-left: 0.5rem; }"
      )
    )
  ),
  shiny::tabPanel(
    "Resumen",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(4, shiny::uiOutput("students_summary")),
        shiny::column(4, shiny::uiOutput("projects_summary")),
        shiny::column(4, shiny::uiOutput("meetings_summary"))
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::wellPanel(
            shiny::h4("Planificación y seguimiento"),
            shiny::p(
              "Utilice esta aplicación para registrar estudiantes, vincularles proyectos, ",
              "asignar directores y documentar reuniones clave. Cada módulo permite cargar datos ",
              "desde archivos CSV existentes y descargar la información actualizada para compartirla ",
              "con el comité académico."
            ),
            shiny::p(
              "Los datos permanecen solamente en esta sesión de Shiny. Para conservarlos, ",
              "descargue los archivos CSV después de realizar cambios."
            )
          )
        )
      )
    )
  ),
  shiny::tabPanel(
    "Estudiantes",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Nuevo estudiante"),
            shiny::textInput("student_name", "Nombre completo"),
            shiny::textInput("student_email", "Correo electrónico"),
            shiny::selectInput(
              "student_program",
              "Programa",
              choices = c(
                "Arquitectura", "Biología", "Ciencias de la Computación",
                "Derecho", "Economía", "Educación", "Ingeniería Industrial",
                "Ingeniería de Sistemas", "Matemáticas", "Psicología"
              ),
              selected = "Ciencias de la Computación"
            ),
            shiny::dateInput(
              "student_start_date",
              "Fecha de inicio",
              value = Sys.Date()
            ),
            shiny::selectInput(
              "student_status",
              "Estado",
              choices = c("Propuesta", "En desarrollo", "Sustentado", "Aplazado"),
              selected = "Propuesta"
            ),
            shiny::actionButton("add_student", "Agregar estudiante", class = "btn btn-primary w-100"),
            shiny::actionButton("reset_student", "Limpiar", class = "btn btn-link w-100")
          ),
          shiny::wellPanel(
            shiny::h4("Importar/Exportar"),
            shiny::fileInput("students_upload", "Cargar estudiantes (CSV)", accept = ".csv"),
            shiny::downloadButton("students_download", "Descargar estudiantes", class = "btn btn-success w-100")
          )
        ),
        shiny::column(
          width = 8,
          shiny::h4("Listado de estudiantes"),
          DT::dataTableOutput("students_table")
        )
      )
    )
  ),
  shiny::tabPanel(
    "Proyectos",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Nuevo proyecto"),
            shiny::textInput("project_title", "Título del proyecto"),
            shiny::selectInput("project_student", "Estudiante", choices = character()),
            shiny::textInput("project_advisor", "Director(a)"),
            shiny::selectInput(
              "project_status",
              "Estado",
              choices = c("Propuesta", "Marco teórico", "Trabajo de campo", "Redacción", "Sustentado"),
              selected = "Propuesta"
            ),
            shiny::dateInput("project_due_date", "Fecha objetivo", value = Sys.Date() + 90),
            shiny::textAreaInput("project_description", "Descripción corta", rows = 4),
            shiny::actionButton("add_project", "Registrar proyecto", class = "btn btn-primary w-100"),
            shiny::actionButton("reset_project", "Limpiar", class = "btn btn-link w-100")
          ),
          shiny::wellPanel(
            shiny::h4("Importar/Exportar"),
            shiny::fileInput("projects_upload", "Cargar proyectos (CSV)", accept = ".csv"),
            shiny::downloadButton("projects_download", "Descargar proyectos", class = "btn btn-success w-100")
          )
        ),
        shiny::column(
          width = 8,
          shiny::h4("Portafolio de proyectos"),
          DT::dataTableOutput("projects_table")
        )
      )
    )
  ),
  shiny::tabPanel(
    "Seguimiento",
    shiny::fluidPage(
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::wellPanel(
            shiny::h4("Registrar reunión"),
            shiny::selectInput("meeting_project", "Proyecto", choices = character()),
            shiny::dateInput("meeting_date", "Fecha", value = Sys.Date()),
            shiny::textInput("meeting_participants", "Participantes"),
            shiny::textAreaInput("meeting_actions", "Acuerdos"),
            shiny::textAreaInput("meeting_comments", "Observaciones", rows = 3),
            shiny::actionButton("add_meeting", "Agregar seguimiento", class = "btn btn-primary w-100"),
            shiny::actionButton("reset_meeting", "Limpiar", class = "btn btn-link w-100")
          ),
          shiny::wellPanel(
            shiny::h4("Importar/Exportar"),
            shiny::fileInput("meetings_upload", "Cargar seguimiento (CSV)", accept = ".csv"),
            shiny::downloadButton("meetings_download", "Descargar seguimiento", class = "btn btn-success w-100")
          )
        ),
        shiny::column(
          width = 8,
          shiny::h4("Historial de reuniones"),
          DT::dataTableOutput("meetings_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  state <- shiny::reactiveValues(
    students = empty_students(),
    projects = empty_projects(),
    meetings = empty_meetings()
  )

  reset_inputs <- function(prefix) {
    switch(
      prefix,
      student = {
        shiny::updateTextInput(session, "student_name", value = "")
        shiny::updateTextInput(session, "student_email", value = "")
        shiny::updateSelectInput(session, "student_program", selected = "Ciencias de la Computación")
        shiny::updateDateInput(session, "student_start_date", value = Sys.Date())
        shiny::updateSelectInput(session, "student_status", selected = "Propuesta")
      },
      project = {
        shiny::updateTextInput(session, "project_title", value = "")
        shiny::updateSelectInput(session, "project_student", selected = NULL)
        shiny::updateTextInput(session, "project_advisor", value = "")
        shiny::updateSelectInput(session, "project_status", selected = "Propuesta")
        shiny::updateDateInput(session, "project_due_date", value = Sys.Date() + 90)
        shiny::updateTextAreaInput(session, "project_description", value = "")
      },
      meeting = {
        shiny::updateSelectInput(session, "meeting_project", selected = NULL)
        shiny::updateDateInput(session, "meeting_date", value = Sys.Date())
        shiny::updateTextInput(session, "meeting_participants", value = "")
        shiny::updateTextAreaInput(session, "meeting_actions", value = "")
        shiny::updateTextAreaInput(session, "meeting_comments", value = "")
      }
    )
  }

  shiny::observeEvent(input$reset_student, reset_inputs("student"))
  shiny::observeEvent(input$reset_project, reset_inputs("project"))
  shiny::observeEvent(input$reset_meeting, reset_inputs("meeting"))

  shiny::observe({
    students_choices <- state$students$nombre
    selected_student <- NULL
    if (!length(students_choices)) {
      students_choices <- character()
    } else {
      selected_student <- students_choices[[1]]
    }
    shiny::updateSelectInput(session, "project_student", choices = students_choices, selected = selected_student)
  })

  shiny::observe({
    project_choices <- state$projects$titulo
    selected_project <- NULL
    if (!length(project_choices)) {
      project_choices <- character()
    } else {
      selected_project <- project_choices[[1]]
    }
    shiny::updateSelectInput(session, "meeting_project", choices = project_choices, selected = selected_project)
  })

  shiny::observeEvent(input$add_student, {
    shiny::req(input$student_name, input$student_email)
    new_student <- data.frame(
      id = next_id(state$students),
      nombre = input$student_name,
      email = input$student_email,
      programa = input$student_program,
      fecha_inicio = sanitize_date(input$student_start_date),
      estado = input$student_status,
      stringsAsFactors = FALSE
    )
    state$students <- rbind(state$students, new_student)
    shiny::showNotification("Estudiante registrado", type = "message")
    reset_inputs("student")
  })

  shiny::observeEvent(input$add_project, {
    shiny::req(input$project_title, input$project_student)
    new_project <- data.frame(
      id = next_id(state$projects),
      titulo = input$project_title,
      estudiante = input$project_student,
      asesor = input$project_advisor,
      estado = input$project_status,
      fecha_entrega = sanitize_date(input$project_due_date),
      descripcion = input$project_description,
      stringsAsFactors = FALSE
    )
    state$projects <- rbind(state$projects, new_project)
    shiny::showNotification("Proyecto registrado", type = "message")
    reset_inputs("project")
  })

  shiny::observeEvent(input$add_meeting, {
    shiny::req(input$meeting_project, input$meeting_date)
    new_meeting <- data.frame(
      id = next_id(state$meetings),
      proyecto = input$meeting_project,
      fecha = sanitize_date(input$meeting_date),
      participantes = input$meeting_participants,
      acuerdos = input$meeting_actions,
      comentarios = input$meeting_comments,
      stringsAsFactors = FALSE
    )
    state$meetings <- rbind(state$meetings, new_meeting)
    shiny::showNotification("Seguimiento registrado", type = "message")
    reset_inputs("meeting")
  })

  shiny::observeEvent(input$students_upload, {
    shiny::req(input$students_upload)
    tryCatch({
      uploaded <- utils::read.csv(input$students_upload$datapath, stringsAsFactors = FALSE)
      expected <- c("id", "nombre", "email", "programa", "fecha_inicio", "estado")
      missing <- setdiff(expected, names(uploaded))
      if (length(missing)) {
        stop(sprintf("Columnas faltantes: %s", paste(missing, collapse = ", ")))
      }
      uploaded <- uploaded[expected]
      uploaded$fecha_inicio <- sanitize_date(uploaded$fecha_inicio)
      state$students <- uploaded
      shiny::showNotification("Archivo de estudiantes cargado", type = "message")
    }, error = function(e) {
      shiny::showNotification(sprintf("Error al cargar estudiantes: %s", e$message), type = "error")
    })
  })

  shiny::observeEvent(input$projects_upload, {
    shiny::req(input$projects_upload)
    tryCatch({
      uploaded <- utils::read.csv(input$projects_upload$datapath, stringsAsFactors = FALSE)
      expected <- c("id", "titulo", "estudiante", "asesor", "estado", "fecha_entrega", "descripcion")
      missing <- setdiff(expected, names(uploaded))
      if (length(missing)) {
        stop(sprintf("Columnas faltantes: %s", paste(missing, collapse = ", ")))
      }
      uploaded <- uploaded[expected]
      uploaded$fecha_entrega <- sanitize_date(uploaded$fecha_entrega)
      state$projects <- uploaded
      shiny::showNotification("Archivo de proyectos cargado", type = "message")
    }, error = function(e) {
      shiny::showNotification(sprintf("Error al cargar proyectos: %s", e$message), type = "error")
    })
  })

  shiny::observeEvent(input$meetings_upload, {
    shiny::req(input$meetings_upload)
    tryCatch({
      uploaded <- utils::read.csv(input$meetings_upload$datapath, stringsAsFactors = FALSE)
      expected <- c("id", "proyecto", "fecha", "participantes", "acuerdos", "comentarios")
      missing <- setdiff(expected, names(uploaded))
      if (length(missing)) {
        stop(sprintf("Columnas faltantes: %s", paste(missing, collapse = ", ")))
      }
      uploaded <- uploaded[expected]
      uploaded$fecha <- sanitize_date(uploaded$fecha)
      state$meetings <- uploaded
      shiny::showNotification("Archivo de seguimiento cargado", type = "message")
    }, error = function(e) {
      shiny::showNotification(sprintf("Error al cargar seguimiento: %s", e$message), type = "error")
    })
  })

  output$students_table <- DT::renderDataTable({
    DT::datatable(
      state$students,
      options = list(pageLength = 5, language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json")),
      rownames = FALSE
    )
  })

  output$projects_table <- DT::renderDataTable({
    DT::datatable(
      state$projects,
      options = list(pageLength = 5, language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json")),
      rownames = FALSE
    )
  })

  output$meetings_table <- DT::renderDataTable({
    DT::datatable(
      state$meetings,
      options = list(pageLength = 5, order = list(list(2, "desc")), language = list(url = "//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json")),
      rownames = FALSE
    )
  })

  output$students_download <- shiny::downloadHandler(
    filename = function() {
      paste0("estudiantes-", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(state$students, file, row.names = FALSE)
    }
  )

  output$projects_download <- shiny::downloadHandler(
    filename = function() {
      paste0("proyectos-", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(state$projects, file, row.names = FALSE)
    }
  )

  output$meetings_download <- shiny::downloadHandler(
    filename = function() {
      paste0("seguimiento-", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      utils::write.csv(state$meetings, file, row.names = FALSE)
    }
  )

  output$students_summary <- shiny::renderUI({
    summary_box("Estudiantes registrados", nrow(state$students), status = "primary")
  })

  output$projects_summary <- shiny::renderUI({
    activos <- if (!nrow(state$projects)) 0L else sum(state$projects$estado != "Sustentado", na.rm = TRUE)
    summary_box("Proyectos activos", activos, status = "success")
  })

  output$meetings_summary <- shiny::renderUI({
    summary_box("Próximas reuniones", format_meeting_status(state$meetings), status = "info")
  })
}

shiny::shinyApp(ui, server)
