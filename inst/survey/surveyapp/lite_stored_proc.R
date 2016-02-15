#rm(list=ls(all=TRUE))
require(R6)
require(stringr)
require(RODBC)
require(sqldf)
require(RSQLite)

SQ <- R6Class(
  "SQ",

  public = list(
    dbs = list(
      QRYS = "bts.sqlite",
      BTS =  "bts.sqlite"
    ),

    params = NULL,        # query parameters
    name = NULL,          # query name
    qry_sql = NULL,    # stored query sql statement
    qry_params = NULL, # stored query parameters
    SQRY = "QRYS", # path to stored queries database
    TDB = "BTS", # path to target database


    initialize = function(name , params,sqryDB,targetDB) {
      #set stored query name & parameters if supplied
      if (!missing(name)) {
        if (self$qry_exists(name)) {
          self$name <- name
        }else{
          stop(paste0("Query ",name," does not exists"))
        }
      }
      if (!missing(params)) {
        self$params <- params
      }

      #set path to stored queries database if supplied
      if (!missing(sqryDB)) {
        self$qrys_db <- self$dbs_get_path(sqryDB)
      }

      #set path to target database if path is supplied or path is not NULL
      if (!missing(targetDB)) {
        self$target_db(targetDB)
      }

      #load stored query if name is provided and there are no duplicates
      temp <- private$qry_get()
      if (!is.null(temp)) {
        #str(temp)
        if (nrow(temp == 1)) {
          self$qry_sql <- temp$qry_sql
          self$qry_params <- temp$qry_params
        }
      }
    },

    dbs_get_path = function(value) {
      if (!missing(value)) {
        temp <- self$dbs[value][[1]]
        if (is.null(temp)) {
          stop("Database not found")
        }
        return(temp)
      }else{
        stop("Please supply database name")
      }
    },

    dbs_get_current_paths = function() {
      temp_sq <- self$dbs[self$SQRY][[1]]
      temp_tg <- self$dbs[self$TDB][[1]]
      cat(paste0("Stored Queries DB = ",temp_sq,
                 "\nTarget DB = ",temp_tg))

    },

    dbs_set_path = function(value) {
      if (!missing(value)) {
        temp_class <- class(value)
        if (temp_class == "list" && length(value) == 1) {
          temp_db_name <- names(value)
          self$dbs[[temp_db_name]] <- value[[1]]

        }else{
          stop("Argument should be a list in the form list(DBNAME='path_to_db')")
        }
      }
    },

    dbs_add_path = function(value) {
      self$dbs_set_path(value)
    },


    #set name
    qry_set_name = function(value) {
      if (!missing(value)) {
        if (self$qry_exists(value)) {
          self$name <- value

          temp <- private$qry_get()
          if (!is.null(temp)) {
            if (nrow(temp == 1)) {
              self$qry_sql <- temp$qry_sql
              self$qry_params <- temp$qry_params
              invisible(self)
            }else{
              stop("Multiple queries with same name.")
            }
          }

        }else{
          stop(paste0("Query ",value," does not exists"))
        }
      }
    },

    #get query name
    qry_get_name = function() {
      return(self$name)
    },

    #set params
    qry_set_params = function(value) {
      if (!missing(value)) {
        self$params <- value
      }
    },


    qry_get_params = function() {
      return(strsplit(self$qry_params,
                      private$params_delimeter)[[1]])
    },

    #get stored
    qry_get_sql = function() {
      return(self$qry_sql)
    },


    qry_set_target_db = function(value) {
      if (!missing(value)) {
        self$target_db <- value
      }
      invisible(self)
    },

    qry_set_db_path = function(value) {
      if (!missing(value)) {
        self$qrys_db(value)
      }
      invisible(self)
    },

    qry_print_sql = function(value) {
      if (missing(value)) {
        cat(self$qry_get_sql())
      }else{
        myqry <-
          paste0("select qry_sql from stored_queries where qry_name='",value,"'")
        temp <- private$qry_run_self_sql(myqry)
        if (nrow(temp) > 0) {
          cat(temp$qry_sql)
        }else{

        }
      }
      invisible(self)
    },

    params_split = function(value) {
      temp <- base::gsub(",","','",value)
      temp <- base::paste0("('",temp,"')")
      return(temp)
    },

    params_replace = function() {
      self$qry_replace_params()
    },

    qry_replace_params = function() {
      temp_sql <- self$qry_get_sql()

      if (is.null(temp_sql) ||
          is.na(temp_sql)) {
        stop("No sql found")
      }

      p <- self$qry_get_params()
      if (!is.na(p) || is.null(p)) {
        for (i in 1:length(p)) {

          #cat(p[i],"=")
          #cat(self$params[[p[i]]],"\n")

          if (stringr::str_detect(p[i],"@s_")) {
            temp_sql <- stringr::str_replace(temp_sql,
                                             p[i],
                                             paste0("'",
                                                    self$params[[p[i]]],
                                                    "'")
                                             )

          }else if (stringr::str_detect(p[i],"@ls_")) {
            temp_sql <- stringr::str_replace(temp_sql,
                                             p[i],
                                             self$params_split(self$params[[p[i]]])
                                             )

          }else if (stringr::str_detect(p[i],"@li_")) {
            temp_sql <- stringr::str_replace(temp_sql,
                                             p[i],
                                             paste0("(",
                                                    self$params[[p[i]]],
                                                    ")")
                                             )

          }else{
            temp_sql <-
              stringr::str_replace(temp_sql, p[i], self$params[[p[i]]])
          }
        }
      }

      #multi-statement break into vector
      #cat(temp_sql)
      if (str_detect(temp_sql,";")) {
        temp_sql <- gsub("[\r\n]","",strsplit(temp_sql,";")[[1]])
      }

      return(temp_sql)
    },

    qry_exec = function() {
      temp <- self$qry_replace_params()
      #cat(temp)
      if (length(temp) == 1) {
        return(private$qry_run_target_sql(temp))
      }else{
        ltemp <- sapply(temp, private$qry_run_target_sql)
        return(ltemp)
      }

    },

    get_delimiter = function() {
      return (private$params_delimeter)
    },

    set_delimiter = function(value) {
      if (!missing(value)) {
        if (class(value) == 'character' && nchar(value) == 1) {
          private$params_delimeter <- value
          invisible(self)
        }else{
          stop("Delimiter should be single character")
        }

      }else{
        stop("Delimiter cannot be empty")
      }
    },

    qry_add = function(sname,sqry,sparam) {
      if (!missing(sname) && !missing(sqry)) {
        if (missing(sparam)) {
          if (stringr::str_detect(sqry,"@")) {
            stop("Missing parameters")
          }else{
            private$qry_run_self_sql(
              paste0(
                "insert into stored_queries (qry_name,qry_sql) values ('",
                sname, "','",
                sqry,  "')"
              )
            )
            invisible(self)
          }#@
        }else{
          private$qry_run_self_sql(
            paste0(
              "insert into stored_queries (qry_name,qry_sql,qry_params) values ('",
              sname, "','",
              sqry,  "','" ,
              sparam,"')"
            )
          )
          invisible(self)
        }
      }else{
        stop("Missing query name or sql statement")
      }#missing
    },#function

    qry_update = function(sname,sqry,sparam) {
      if (!missing(sname)) {
        if (missing(sparam) && !missing(sqry)) {
          private$qry_run_self_sql(
            paste0(
              " update stored_queries set ",
              " qry_sql='",sqry, "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else if (!missing(sparam) && missing(sqry)) {
          private$qry_run_self_sql(
            paste0(
              " update stored_queries set ",
              " qry_params='",sparam, "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else if (!missing(sparam) && !missing(sqry)) {
          private$qry_run_self_sql(
            paste0(
              " update stored_queries set ",
              " qry_sql='",sqry, "', ",
              " qry_params='",sparam,  "' where ",
              " qry_name='",sname,"';"
            )
          )
          invisible(self)

        }else{
          stop("Missing sql statement or parameters")
        }#missing
      }else{
        stop("Missing stored query name")
      }
    },#function

    qry_delete = function(value) {
      if (!missing(value)) {
        private$qry_run_self_sql(paste0("delete from stored_queries where qry_name='",value,"';"))
        invisible(self)
      }else{
        stop("Please supply Query name")
      }
    },

    qry_list = function() {
      private$qry_run_self_sql("select qry_name from stored_queries order by qry_name")
    },

    qry_exists = function(value) {
      temp <- private$qry_run_self_sql(
        paste0(
          "select count(qry_name) as scount from stored_queries where qry_name='",value,"'"
        )
      )
      return(as.numeric(temp["scount"]) > 0)
    },

    tqry_exec = function(value) {
      if (!missing(value)) {
        private$qry_run_target_sql(value)
      }else{
        stop("Please supply sql statement")
      }

    },

    table_import = function(src,dst,tbl) {
      dbh <- odbcDriverConnect(src)

      src_table <- sqlQuery(dbh,paste0("select * from ",tbl))
      dst_db <- RSQLite::dbConnect(SQLite(), dbname = dst)
      RSQLite::dbWriteTable(conn = dst_db, name = tbl, value = src_table)

      close(dbh)
    },

    tables_list = function(value) {
      tbls <- NULL
      if (!missing(value)) {
        self$TDB <- value
      }

      tbls <-
        private$qry_run_target_sql(paste0("SELECT name FROM sqlite_master WHERE type = 'table'"))

      return(tbls)
    },

    tables_count = function(value) {
      tbls <- self$tables_list(value = value)
      return(nrow(tbls))
    },

    table_get_records = function(tname,n,top = TRUE,where=NULL) {
      if (!missing(tname)) {
        temp <- NULL
        if(is.null(where)){
           temp <- private$qry_run_target_sql(paste0("select * from ",tname))
        }else{
          temp <- private$qry_run_target_sql(
                     paste0(
                        "select * from ",tname,
                        " where ",where
                     )
                  )
        }
        if (!missing(n)) {
          if (top) {
            return(head(temp,n))
          }else{
            return(tail(temp,n))
          }
        }
        return(temp)
      }else{
        stop("Please supply the table name")
      }
    },

    table_head = function(tname, n = 10,where=NULL) {
      temp <- self$table_all(tname,where = where)
      if (nrow(temp) > 0) {
        return(head(temp,n = n))
      }else{
        return(temp)
      }
    },

    table_tail = function (tname,n = 10,where=NULL) {
      temp <- self$table_all(tname,where = where)
      if (nrow(temp) > 0) {
        return(tail(temp,n = n))
      }else{
        return(temp)
      }
    },

    table_split_name = function(tname) {
      temp <- tname
      if (str_detect(tname,"::")) {
        temp <- str_split(tname,"::")[[1]]
      }
      return(temp)
    },

    table_all = function(tname,where=NULL) {
      if (!missing(tname)) {
        temp <- self$table_split_name(tname)
        temp_name <- tname

        if (length(temp) > 1) {
          self$TDB <- temp[1]
          temp_tname <- temp[2]
        }

        return(self$table_get_records(tname = temp_tname,where=where))
      }else{
        stop("Please supply table name")
      }
    },

    table_info = function(tname) {
      if (!missing(tname)) {
        temp <- self$table_split_name(tname)
        temp_tname <- tname
        if (length(temp) > 1) {
          self$TDB <- temp[1]
          temp_tname <- temp[2]
        }


        return(private$qry_run_target_sql(paste0(
          "PRAGMA table_info('",temp_tname,"')"
        )))

      }else{
        stop("Please supply the table name")
      }
    },

    table_add = function(tname,data) {
      temp <- self$table_split_name(tname)
      temp_tname <- tname
      if (length(temp) > 1) {
        self$TDB <- temp[1]
        temp_tname <- temp[2]
      }
      self$table_create(temp_tname,df = data)
    },

    table_copy = function(tname,db_to,db_from = NULL) {
      if (!(missing(tname) || missing(db_to))) {
        temp <- self$table_split_name(tname)
        if (length(temp) > 1) {
          data <- self$table_all(tname)
          dest_name <- paste0(db_to , "::" , temp[2])
          self$table_add(dest_name,data)

        }else if (!is.null(db_from)) {
          src_name <- paste0(db_from , "::" , tname)
          dest_name <- paste0(db_to , "::" , tname)
          data <- self$table_all(src_name)
          self$table_add(dest_name,data)

        }else{
          stop("Please supply source database name:db_from")
        }
      }
    },

    table_create = function (tname,df) {
      RSQLite::dbWriteTable(
        conn = RSQLite::dbConnect(SQLite(), dbname = self$target_db),
        name = tname,
        value = df
      )

    },

    table_update = function(tname,set,where){

      if (!missing(tname) && !missing(set) && !missing(where)) {
        temp <- self$table_split_name(tname)
        temp_tname <- tname

        if (length(temp) > 1) {

           self$TDB <- temp[1]
          temp_tname <- temp[2]
          #cat(temp)
        }
        qry <- paste0(
            " update ", temp_tname,
            " set    ", set,
            " where  ", where
        )

        private$qry_run_target_sql(qry)
        #cat(qry)
        invisible(self)
      }else{
        stop(" Missing parameter(s): tname/set/where")
      }
    },

    table_insert = function(tname,fields,values){

      if (!missing(tname) && !missing(fields) && !missing(values)) {
        temp <- self$table_split_name(tname)
        temp_tname <- tname

        if (length(temp) > 1) {
          self$TDB <- temp[1]
          temp_tname <- temp[2]
          #cat(temp)
        }
        qry <- paste0(
          " insert into ", temp_tname,
          " (", fields,")",
          " values (", values,")"
        )

        private$qry_run_target_sql(qry)
        #cat(qry)
        invisible(self)
      }else{
        stop(" Missing parameter(s): tname/fields/values")
      }
    },



    table_select = function(tname,fields=" * ",where,n){

      if (!missing(tname)) {
        temp <- self$table_split_name(tname)
        temp_tname <- tname

        if (length(temp) > 1) {
          self$TDB <- temp[1]
          temp_tname <- temp[2]
          #cat(temp)
        }

        temp_where <-""
        if( !missing(where)){
          temp_where <- paste0(" where ",where)
        }

        temp_n <- ""
        if(!missing(n)){

          temp_n <- paste0(" limit ",n)
        }

        qry <- paste0(
          " select ",fields," from ", temp_tname, temp_where, temp_n
        )

        return (
            private$qry_run_target_sql(qry)
        )
        #cat(qry)
      }else{
        stop(" Missing parameter(s): tname")
      }
    },

    view_ms_con = function() {
      return(private$get_mssql_con())
    },


    qry_run_ms = function(value) {
      dbh <- odbcDriverConnect(private$get_mssql_con())
      result <- sqlQuery(dbh, value)
      close(dbh)

      return (result)
    }

  ),

  active = list(
    qrys_db = function(value) {
      if (missing(value) || is.null(value) || is.na(value)) {
        return(self$dbs_get_path(self$SQRY))
      }else{
        self$SQRY <- value
        #return(self$dbs_get_path(value))
      }
    },

    target_db = function(value) {
      if (missing(value) || is.null(value) || is.na(value)) {
        return(self$dbs_get_path(self$TDB))

      }else{
        self$TDB <- value
        #return(self$dbs_get_path(value))
      }
    }
  ),

  private = list(
    params_delimeter = "," ,

    qry_get = function() {
      if (!is.null(self$name)) {
        private$qry_run_self_sql(paste0(
          "select * from stored_queries where qry_name='", self$name ,"'"
        ))
      }else{
        #stop(paste0("Query name is NULL"))
        return(NULL)
      }#if
    },#function

    qry_run_target_sql = function(value) {
      #cat("Eentering Target environment")
      return(sqldf::sqldf(value,dbname =  self$target_db))
    },

    qry_run_self_sql = function(value) {
      #cat("Eentering StoredvQueries environment")
      return(sqldf::sqldf(value,dbname =  self$qrys_db))

    },

    qry_create_self_table = function() {
      qry_ddl <- "
      CREATE TABLE stored_queries (
      id    INTEGER      PRIMARY KEY AUTOINCREMENT NOT NULL,
      qry_name           VARCHAR (50),
      qry_params         TEXT,
      qry_sql            TEXT,
      qry_default_values TEXT
      );
      "

      checks_sql <-
        "SELECT count(name) as tcount FROM sqlite_master WHERE type='table' AND name='stored_queries';"
      private$qry_create_self_db()
      tcount <- private$qry_run_self_sql(checks_sql)

      if (nrow(tcount) == 0) {
        ttable <- private$qry_run_self_sql(qry_ddl)
      }

      invisible(self)
    },

    qry_create_self_db = function(value = "StoredProcsDB.sqlite") {
      if (is.null(self$qrys_db)) {
        db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = value)
        self$qrys_db <- value
      }
      invisible(self)
    }


)
)



#sq <- SQ$new()
#sq$table_tail("BINDX::beama_indices")
#sq$table_update("BINDX::beama_indices", set="qtr=2", where=" issue=550 and mth=5")
#sq$table_copy("BNW::sankey_data","BINDX")
#sq$table_select("BINDX::beama_indices",fields="yr,mth,index_name,issue,index_base",n=10)
#`rm(list=ls(all=TRUE))

