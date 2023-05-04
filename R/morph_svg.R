#' Quarto SVG Morph
#'
#' @description This function creates animations from two or more SVG files to display in Quarto or other R-generated HTML-Documents. It takes folder names or file names as inputs.
#'
#' @param ... Filenames: Add two or more file names incl. paths. Can also be a vector with filenames. The function must be called with the path to at least 2 .svg files or a folder (see next parameter).
#' @param folder Alternative to using several file names. .svg images will be loaded in alphabetic order from the folder.
#' @param height Height of the SVG file. Defaults to "auto" which should correspond to the height the svg document is saved in. Default is pixels, can be any css size attribute, e.g. "12em", "5px".
#' @param img_id Optional internal id, defaults to time stamp.
#' @param animation_duration Optional duration of animation in seconds. Defaults to 0.2.
#' @param animation_curve HTML animation curve. Can be "linear", "ease", "ease-in", "ease-out", "ease-in-out", or any cubic-bezier function. Defaults to: "cubic-bezier(0.76, 0, 0.24, 1)".
#' @param click Whether animation is clickable, e.g. for bookdown documents, websites, etc. Defaults to TRUE. In Quarto presentations animations follow the reveal structure and click can be turned off if undesirable by setting to FALSE.
#' @param new_ids Removes existing ids. Not recommended.
#' @return Returns the animated SVG as HTML-Code at the location where the function is called.
#' @export
#' @examples
#' morph_svg(
#'   "https://raw.githubusercontent.com/klingelhoefer/quartoMorphSvg/master/tests/images/mediation/Slide1.svg",
#'   "https://raw.githubusercontent.com/klingelhoefer/quartoMorphSvg/master/tests/images/mediation/Slide2.svg"
#'   )

morph_svg = function(
    ..., # svg paths  as strings, can be a strings or a vector (order matters)
    folder = "",
    height = "auto",
    img_id = toString(as.numeric(Sys.time())*1000000),
    animation_duration = .2,
    animation_curve = "cubic-bezier(0.76, 0, 0.24, 1)",
    click = T,
    new_ids = F
){
  # collecting filenames
  filenames = c(...)

  # generating relevant strings
  animation_duration = paste0(animation_duration, "s")
  img_id = paste0("s_", img_id)
  placeholder = "A_PLACEHOLDER_A" # placeholder
  if(is.numeric(height)){height = paste0(height, "px")} # height in pixels

  if(nchar(toString(folder)) > 0 & nchar(toString(filenames)) == 0) {
    # if only a folder is provided, assume that all .svg files in the folder are targets
    # assumes alphabetical order
    # e.g. name your files "a.svg", "b.svg", "c.svg" to get the correct order
    warning(paste("No filenames supplied \n Matching all files in folder",
                  paste0("'",folder,"'"),
                  "\n Ignore if this is correct"))
    f = gsub("//+", "/", #removing redundant slashes
             paste0(folder, "/",
                    grep("\\.[sS][vV][gG]$", list.files(folder), value = T
                    )))
  }
  if(nchar(toString(folder)) > 0 & nchar(toString(filenames)) > 0) {
    # stringing folder and filenames together
    f = gsub("//+", "/", # removing redundant "/"
             paste0( folder, "/",
                     grep(".svg$", filenames, value = T
                     )))
  }
  if(nchar(toString(folder)) == 0){
    # gettting filenames if no folder is supplied
    f = grep(".svg$", filenames, value = T)
  }

  # checking and applying creating vector
  if(length(f) < 2){stop("provide 2 or more svg files with '.svg' file extension or check folder path")}
  svgs = sapply(1:length(f), function(x){
    a = f[x]
    names(a) = paste0("t", x)
    a
  })

  # creating container to save variables
  attr = list()

  # looping through all svgs
  for(s_i in 1:length(svgs)){

    # reading svg
    t = gsub("\\\"", "'", # replacing all " with '
             readLines(svgs[s_i]) #reading svg
    )
    t = gsub("<!--[\\s\\S]*?-->", "", t) # removing comments

    # extracting width of the svg (assuming the first width argument is that of the canvas)
    w = regmatches(t, regexpr("\\s*width\\s*=\\s*'(.*?)'", t))[1] #getting
    w = regmatches(w, regexpr("[+ -]?[0-9]+([.][0-9]+)?", w)) # extracting number

    # extracting height of the svg (assuming the first height argument is that of the canvas)
    h = regmatches(t, regexpr("\\s*height\\s*=\\s*'(.*?)'", t))[1] #getting
    h = regmatches(h, regexpr("[+ -]?[0-9]+([.][0-9]+)?", h)) # extracting number

    # synthesizing viewbox
    viewbox = paste0("viewbox='0 0 ", w, " ", h, "' ")

    # extracting viewbox if it is present

    if(sum(grepl("\\s*(viewBox|viewbox)\\s*=\\s*'(.*?)'", t)) > 0) {
      viewbox =
        regmatches(t, regexpr("\\s*(viewBox|viewbox)\\s*=\\s*'(.*?)'", t))[1]
    }

    # collapsing into one string
    t = paste0(t, collapse = "")

    # setting viewbox if it does not exist
    if(sum(grepl("viewbox\\s*=\\s*", t)) == 0){
      t = sub("<svg ", paste0("<svg ", viewbox, " "), t)
    }

    # splitting by tag
    # setting placeholder at split positions
    t = gsub("<", paste0(placeholder, "<"), t)
    #removing redundant whitespaces (>1)
    t = gsub("\\s\\s+", " ", t)
    # removing by splitting
    t = c(unlist(strsplit(t, placeholder)))


    # giving data attribute to each tag
    t = lapply(
      1:length(t),
      function(x) {
        s = c(t[x], "", "")
        if(!grepl("</", t[x]) & grepl("<[^>]*>", t[x])){
          s = unlist(strsplit(t[x], "<"))  # splitting in parts by "<"
          s_x = sub("\\s", placeholder, s[2]) |>
            strsplit(placeholder) |>
            unlist()
          s[2] = s_x[1]; s[3] = s_x[2] # adding splitted parts
          # if no attributes
          if(length(s) < 3){
            s_x = gsub(">", paste0(placeholder, ">"), s[2]) |>
              strsplit(placeholder) |>
              unlist()
            s[2] = s_x[1] # adding adding together
            s[3] = s_x[2]
          }
          s[1] = "<"
        }
        s
      }
    )
    # turning to dataframe
    t = as.data.frame(do.call(rbind, t), check.names = F, fix.empty.names = F)
    #giving names
    colnames(t) = c("start", "node", "end")
    # giving elements numbers
    if(length(unique(t$node[t$node != "" & t$node!="svg"])) > 0){
      for(node in unique(t$node[t$node != "" & t$node!="svg"])){
        node_number = 0 # resetting
        for(i_r in 1:nrow(t)){
          if(t[i_r, "node"] == node){
            t[i_r, "data_id"] = paste0(img_id, "_", node, "_", node_number)
            t[i_r, "node_number"] = paste0(node_number)
            node_number = node_number + 1
          }
        }
      }
    } else {t$data_id = NA}

    # getting original id
    t$og_id = sapply(
      paste(t$start, t$node, t$end),
      function(x){
        a = unlist(regmatches(x, regexpr("id\\s*=\\s*'(.*?)'", x)))
        a = gsub("'", "", gsub("id\\s*=\\s*'", "", a))
        a = ifelse(length(a) > 0, a, "")
      }
    )

    # making ids unique
    t$og_id = paste0(img_id, "_", t$og_id)

    # making ids unique for svg text
    t$end = gsub("id\\s*=\\s*'", paste0("id='", img_id, "_"), t$end)


    # setting NA ids to placeholder
    t$data_id[is.na(t$data_id)] =
      paste0(placeholder, "_", 1:length(t$data_id[is.na(t$data_id)]), "_", s_i)

    # getting data ids from each tag (if new ids are not desired)
    if(!new_ids){
      t["data_id"] = sapply(
        1:nrow(t),
        function(x){
          ifelse(
            grepl("id\\s*=\\s*'(.*?)'", paste(t$start[x], t$end[x])),
            gsub("'", "", gsub("id\\s*=\\s*'", "", regmatches(
              paste(t$start[x], t$end[x]),
              regexpr("id\\s*=\\s*'(.*?)'", paste(t$start[x], t$end[x]))))),
            t$data_id[x]
          )
        }
      )
    }

    # giving svg tag data-id and regular id to be addressed individually
    t$data_id[t$node == "svg"] = img_id
    t$end[t$node == "svg"] =
      gsub("id\\s*=\\s*'(.*?)'", "", t$end[t$node == "svg"])
    t$end[t$node == "svg"] =
      paste0("id='", img_id, "' ", t$end[t$node == "svg"])
    # removing width and height attribute to allow scaling from within function
    t$end[t$node == "svg"] =
      gsub("\\s+width\\s*=\\s*'(.*?)'", "", t$end[t$node == "svg"])
    t$end[t$node == "svg"] =
      gsub("\\s+height\\s*=\\s*'(.*?)'", "", t$end[t$node == "svg"])

    #changing image ids to allow for multiple images with the same id within a presentation
    t$og_id[t$node == "image"] = paste0(img_id, "_", t$og_id[t$node == "image"])
    t$end[t$node == "image"] =
      gsub("id\\s*=\\s*'(.*?)'", paste0("id='",img_id,"_\\1'"), t$end[t$node == "image"])
    # changing use hrefs to match images
    t$end[t$node == "use"] =
      gsub("href\\s*=\\s*'\\s*#(.*?)'", paste0("href='#",img_id,"_\\1'"), t$end[t$node == "use"])

    # integrating everything
    t$out = sapply(
      1:nrow(t),
      function(x) {
        paste0(
          t[x, "start"],
          t[x, "node"], " ",
          ifelse(
            !grepl(placeholder, t$data_id[x]),
            paste0("data-idd='",t[x, "data_id"],"' "),
            ""),
          " ",
          t[x, "end"]
        )
      }
    )

    # only selecting non-emtpy columns
    t = t[t$start != "",]

    # adding the svg to dataframe
    attr[[s_i]] = data.frame("svg" = t$out, check.names = F, fix.empty.names = F)

    #adding ordering variable, node variable, original id variable
    attr[[s_i]]["order"] = 1:length(attr[[s_i]][["svg"]])
    attr[[s_i]]["node"] = t$node
    attr[[s_i]]["og_id"] = t$og_id
    attr[[s_i]]["node_number"] = t$node_number

    # adding ids from data_id
    attr[[s_i]]["data_id"] = t$data_id

    # extracting position (x, y)
    attrs = c("x", "y")
    for(a in attrs) {
      # getting attribute x/y position
      attr[[s_i]][a] = sapply(
        1:nrow(attr[[s_i]]),
        function(x){
          r = regmatches(
            attr[[s_i]][["svg"]][x],
            regexpr(
              paste0("(?:\\s|')",a,"\\s*=\\s*'.*?'"),
              attr[[s_i]][["svg"]][x]
            ))
          r = regmatches(r, regexpr("'.*?'", r))
          r = gsub("'", "", r)
          if(length(r) == 0){r = 0}
          if(is.na(r)){r = 0}
          r
        })

      # change percentages to numbers
      attr[[s_i]][[a]][grepl("%", attr[[s_i]][[a]])] =
        as.numeric(
          gsub("\\s", "", gsub("%", "",
                               attr[[s_i]][[a]][grepl("%", attr[[s_i]][[a]])]
          )))/100
      #change pixels to numbers
      attr[[s_i]][[a]][grepl("px", attr[[s_i]][[a]])] =
        as.numeric(
          gsub("\\s", "", gsub("px", "",
                               attr[[s_i]][[a]][grepl("px", attr[[s_i]][[a]])]
          )))
    }

    # extracting transformations
    for(i_tr in 1:nrow(attr[[s_i]])){
      transform_i =
        gsub("'", "", sub("\\s*transform\\s*=\\s*", "",
                          regmatches(
                            attr[[s_i]][["svg"]][i_tr],
                            regexpr("transform\\s*=\\s*'(.*?)'",
                                    attr[[s_i]][["svg"]][i_tr]))))
      # removing NAs / character 0
      transform_i[is.na(transform_i)] = ""
      if(length(transform_i) == 0){transform_i = ""}

      # calculating translations (needed to animate position correctly)
      if(length(grepl("translate\\s*\\(", transform_i))!= 0){
        # extracting translation(s)
        translate_i = regmatches(
          transform_i,
          gregexpr("translate\\s*\\([^)]*\\)",
                   transform_i)) |> unlist()

        # getting x position
        tr_x = gsub("\\(|,|\\s", "", regmatches(
          translate_i,
          gregexpr("\\(\\s*(.*?)\\s*(,|\\s)",
                   translate_i)) |> unlist()) |>
          as.numeric() |> sum()

        # getting y position
        tr_y = gsub("\\)|,|\\s", "", regmatches(
          translate_i,
          gregexpr("(\\s|,)(.*?)\\)",
                   translate_i)) |> unlist()) |>
          as.numeric() |> sum()

        #removing old translate values
        transform_i = gsub("\\s\\s", "",
                           gsub("translate\\([^)]*\\)", "", transform_i))
      } else {
        tr_x = 0 # zero if no translation
        tr_y = 0
      }

      # saving for later use
      attr[[s_i]][["x_translate"]][i_tr] = tr_x
      attr[[s_i]][["y_translate"]][i_tr] = tr_y

      # adding other transformations
      if(length(transform_i)==0){transform_i = ""}
      attr[[s_i]][["transform"]][i_tr] = transform_i
    }


    # renaming variables to reflect time (t1, t2, ...)
    attr[[s_i]]["id_t"] = attr[[s_i]]["data_id"]
    names(attr[[s_i]])[names(attr[[s_i]]) != "data_id"] =
      paste0(names(attr[[s_i]])[names(attr[[s_i]]) != "data_id"],"___t",s_i)
  }

  # trying to match
  for(s_i in 1:length(svgs)){
    # for first frame
    if(s_i == 1){
      # setting up ids
      attr[[1]]["match_id"] = 1:nrow(attr[[1]])
      attr[[1]]["match_id"] = ifelse(
        attr[[1]][["og_id___t1"]] != "",
        attr[[1]][["og_id___t1"]],
        attr[[1]][["match_id"]]
      )
      diff = attr[[1]] # setting up df
      diff$match_id = paste0(img_id,"_",diff$match_id)
      attr[[1]]["match_id"] = paste0(img_id,"_",diff$match_id)


      # splitting svg in words
      w_a = lapply(
        attr[[1]][["svg___t1"]],
        function(x) {
          a = strsplit(x, "\\s|>|:|=")[[1]]
          a = gsub("^\\s", "", a)
          a = a[a != "" & a != " "]
        }
      )
    }
    # for second frame onward
    if(s_i > 1){
      # creating collector df
      match_scores = data.frame()
      attr[[s_i]][paste0("matched_t",s_i)] = F #resetting matches
      id_match_df = data.frame()
      df_i = data.frame()

      for(r_i in 1:length(w_a)){

        # only for defined nodetypes
        if(!is.na(attr[[1]][r_i, "node___t1"]) & attr[[1]][[r_i, "node___t1"]] != ""){

          # getting only matches with same type
          df_i = attr[[s_i]][attr[[1]][[r_i, "node___t1"]] == attr[[s_i]][paste0("node___t",s_i)],]
          df_i = df_i[!is.na(df_i[paste0("svg___t",s_i)]),]
          c = unlist(df_i[paste0("matched_t",s_i)] == F)
          # only not previously matched rows
          df_i = df_i[c,]
          rownames(df_i) = NULL # resetting rownames

          if(nrow(df_i) != 0){
            # splitting svg at tx in words
            w_b = lapply(
              df_i[[paste0("svg___t", s_i)]],
              function(x) {
                a = strsplit(x, "\\s|>|:|=")[[1]]
                a = gsub("^\\s", "", a)
                a = a[a != "" & a != " "]
              }
            )

            # checking if matches original id
            og_id = attr[[1]][[r_i, "og_id___t1"]]
            if(og_id != ""){
              id_match = ifelse(
                og_id %in% attr[[s_i]][[paste0("og_id___t", s_i)]],
                rownames(attr[[s_i]][attr[[s_i]][[paste0("og_id___t", s_i)]] == og_id,]),
                ""
              )
              attr[[s_i]][id_match, paste0("matched_t",s_i)] = T # setting flag for match found
            } else {
              # getting matches
              matches = sapply(
                1:length(w_b),
                function(x) {
                  ifelse(
                    sum(grepl("</", w_a[[r_i]])) == 0,
                    length(intersect(w_a[[r_i]], w_b[[x]])),
                    0
                  )
                }
              )
              # if no conflict exists
              if(length(which(matches == max(matches))) == 1 & sum(matches) > 0){
                id_match = which(attr[[s_i]][["data_id"]]  == df_i[which(matches == max(matches)),"data_id"])
                attr[[s_i]][id_match, paste0("matched_t",s_i)] = T # setting flag
              }
              # if conflict exists
              if(length(which(matches == max(matches))) > 1 & sum(matches) > 0){
                m_i = which(matches == max(matches))[1] # matching first occurence
                id_match = which(attr[[s_i]][["data_id"]]  == df_i[m_i,"data_id"])
                attr[[s_i]][id_match, paste0("matched_t",s_i)] = T # setting flag
              }
            }
          } else {id_match = NA}
        } else {id_match = NA}
        id_match_df[r_i, "match_row_t1"] = as.numeric(r_i)
        id_match_df[r_i, "match_row_tx"] = as.numeric(id_match)
      }

      for(j in id_match_df[!is.na(id_match_df$match_row_tx), "match_row_tx"]){
        attr[[s_i]][j,"match_id"] =
          diff$match_id[id_match_df$match_row_tx[which(id_match_df$match_row_tx == j)]][1] #only selecting first match
      }

      # getting only relevant columns
      df_y = attr[[s_i]][!is.na(attr[[s_i]]["match_id"]),]

      diff = merge(x = diff, y = df_y, by = "match_id", all.x = T)
      duplicated(df_y)
      diff = diff[order(diff$order___t1),] # ordering
      rownames(diff) = diff$order___t1
    }
  }

  diff["data_id"] = diff["match_id"]

  diff["data_id"] = sapply(
    1:nrow(diff),
    function(x){
      a = if(!grepl("</", diff$svg___t1[x])){diff[["data_id"]][x]} else
      {paste0(placeholder,"_",x)}
      a
    }
  )

  diff[["svg___t1"]] = sapply(
    1:length(diff[["svg___t1"]]),
    function(x) {
      ifelse(
        grepl("data-idd='(.*?)'", diff[["svg___t1"]][x]),
        gsub("data-idd='(.*?)'",
             paste0(" data-idd='",diff[["data_id"]][x],"' "),
             diff[["svg___t1"]][x]),
        ifelse(
          !grepl("</", diff[["svg___t1"]][x]),
          gsub("<(\\S+)\\s",
               paste0("<\\1 data-idd='", diff[["data_id"]][x],"' "),
               diff[["svg___t1"]][x]),
          diff[["svg___t1"]][x]
        )
      )
    }
  )


  diff["data_id.x"] = NULL


  # joining to dataframe
  diff = Reduce(function(x, y) merge(x, y, by = "data_id", all = TRUE), attr)
  # ordering by order at t1
  diff = diff[order(diff$order___t1),]


  diff = diff[order(diff$order___t1),] #ordering




  # # checking number of matches
  # id_check = sapply(
  #   2:length(svgs),
  #   function(x){
  #     substr(diff[[paste0("svg___t", x)]], 0, 250)
  #   }
  # )
  # id_check = sapply(
  #   1:nrow(id_check),
  #   function(x){
  #     sum(is.na(id_check[x,])) == 0
  #   }
  # )
  # if(!sum(id_check) > nrow(diff)/2*length(svgs)*.5){
  #   warning("Detected less than 50% matches. Make sure to open svg in inkscape then save as inkscape svg, then copy and edit the next frame(s) to ensure matching ids in all frames")
  # }
  # rm(id_check)

  for(i_a in 1:length(attr)){
    # getting x and y transformations
    ## getting list of current position
    x_tx = paste0("x___t", i_a)
    y_tx = paste0("y___t", i_a)
    transform_tx = paste0("transform___t", i_a)
    x_translate_tx = paste0("x_translate___t", i_a)
    y_translate_tx = paste0("y_translate___t", i_a)
    x_at_tx = paste0("x___at_t", i_a)
    y_at_tx = paste0("y___at_t", i_a)

    # setting NA to 0
    diff[x_tx][is.na(diff[x_tx])] = 0
    diff[y_tx][is.na(diff[y_tx])] = 0
    diff[transform_tx][is.na(diff[transform_tx])] = 0
    diff[x_translate_tx][is.na(diff[x_translate_tx])] = 0
    diff[y_translate_tx][is.na(diff[y_translate_tx])] = 0

    # calculating difference score
    if(i_a == 1){ # for first frame
      diff[x_at_tx] = as.numeric(diff[[x_translate_tx]])
      diff[y_at_tx] = as.numeric(diff[[y_translate_tx]])
    }
    if(i_a > 1){ # for second svg onwards
      diff[x_at_tx] = as.numeric(diff[[x_tx]]) -
        as.numeric(diff[["x___at_t1"]]) +
        as.numeric(diff[[x_translate_tx]])
      diff[y_at_tx] = as.numeric(diff[[y_tx]]) -
        as.numeric(diff[["y___at_t1"]]) +
        as.numeric(diff[[y_translate_tx]])
    }
    # replacing zeros
    diff[x_at_tx][is.na(diff[x_tx])] = 0
    diff[y_at_tx][is.na(diff[y_tx])] = 0

    for(a_pos in c(x_at_tx, y_at_tx, x_translate_tx, y_translate_tx)){
      # change percentages to numbers
      diff[[a_pos]][grepl("%", diff[[a_pos]])] =
        as.numeric(
          gsub(
            "\\s", "", gsub(
              "%", "",
              diff[[a_pos]][grepl("%", diff[[a_pos]])]
            )))/100
      #change pixels to numbers
      diff[[a_pos]][grepl("px", diff[[a_pos]])] =
        as.numeric(
          gsub(
            "\\s", "", gsub(
              "px", "",
              diff[[a_pos]][grepl("px", diff[[a_pos]])]
            )))
    }

    ## transformations and integrating position
    ### note: necessary to use translate, as position is not animateable
    ## translate
    if(i_a == 1){ # for first one
      diff[paste0("s___transform_t", i_a)] = sapply(
        1:nrow(diff),
        function(x){
          paste0(
            "if(document.querySelector('[data-idd=\"",diff$data_id[x],"\"]') != null){",
            "document.querySelector('[data-idd=\"",
            diff$data_id[x],"\"]').setAttributeNS(null, 'transform','",
            "translate(",
            paste0(diff[[x_translate_tx]][x]), ", ",
            paste0(diff[[y_translate_tx]][x]), ") ",
            diff[[transform_tx]][x],
            "'); ",
            "} "
          )
        }
      )
    }
    if(i_a > 1){ # for subsequent ones
      diff[paste0("s___transform_t", i_a)] = #only selecting transform at tx
        sapply(
          1:nrow(diff),
          function(x){
            ifelse(
              !is.na(diff[x, paste0("matched_t",i_a)]) & diff[x, paste0("matched_t",i_a)],
              paste0(
                "if(document.querySelector('[data-idd=\"",diff$data_id[x],"\"]') != null){",
                "document.querySelector('[data-idd=\"",
                diff$data_id[x],"\"]').setAttributeNS(null, 'transform','",
                "translate(",
                as.numeric(diff[[x_tx]][x]) -
                  as.numeric(diff[["x___t1"]][x]),
                ", ",
                as.numeric(diff[[y_tx]][x]) -
                  as.numeric(diff[["y___t1"]][x]),
                ") ",
                # adding position
                "translate(",
                diff[[x_translate_tx]][x], ", ", diff[[y_translate_tx]][x],
                ") ", # adding transformation
                ifelse(diff[[transform_tx]][x] != "0", diff[[transform_tx]][x], ""),
                "'); ",
                "} "
              ),
              paste0("")
            )

          }
        )
    }

    #  getting other attributes
    # getting attributes excluding x, y, style, id, class
    attr_c_i_a = sapply(
      diff[[paste0("svg___t",i_a)]],
      function(x){
        r = regmatches(
          x,
          gregexpr(
            "([\\s'])((?:(?!x\\b|y\\b|id\\b|class\\b|transform\\b|data-idd\\b)[\\w\\-:\\s])*)=\\s*('[^']*')", perl = T,
            x)) |> unlist()
        #setting string to empty if no attributes
        if(length(r) == 0){r = ""}
        r
      }
    )
    # getting attribute name
    attr_c_i_n = sapply(
      attr_c_i_a,
      function(x){
        r = gsub("('|=|\\s)", "", regmatches(
          x,
          regexpr(
            "[\\w\\-:\\s]+=",
            x, perl = T)) |>
            unlist())
        if(length(r) == 0){r = ""}
        r
      }
    )

    # collecting unique attribute names
    attr_c_i = unique(unlist(attr_c_i_n))
    attr_c_i = attr_c_i[!is.na(attr_c_i)] #remove na
    #removing unsupported names
    attr_c_i = gsub(":", "", gsub("^.*?(?=:)", "", attr_c_i, perl = T))

    # adding to global collection variable
    if(i_a == 1){attr_c = attr_c_i}
    if(i_a > 1){attr_c = c(attr_c, attr_c_i)}

    # looping through attributes of svg elements
    for(a in attr_c){
      ## getting attribute
      r = sapply(
        diff[[paste0("svg___t", i_a)]],
        function(x){
          r_i = regmatches(
            x, regexpr(paste0("\\s+",a,"\\s*=\\s*'[^']*'"),
                       perl = T, x)) |> unlist()
          r_i = regmatches(
            r_i, regexpr("'[\\s\\S]*?'",
                         perl = T,
                         r_i))
          if(length(r_i)==0){r_i = ""}
          r_i
        })
      # removing unnecessary parts of the string
      r = gsub("'", "", r)

      # adding to df
      diff[[paste0("attr___", a, "___t", i_a)]] = paste(r)
    }
  }

  # making all attributes into functions (except style)
  for(i_a in 1:(length(svgs))){
    # getting all changes except style, transform, and some metadata, e.g. xmlns
    for(a in attr_c[!grepl("(style|xmlns)", attr_c)]){
      # collecting short names
      n = paste0("attr___", a, "___t", i_a)
      if(!n %in% names(diff)){diff[n] = ""}
      # setting NA to empty string
      for(ii in 1:nrow(diff)){diff[[n]][ii] = ifelse(is.na(diff[[n]][ii]), "", diff[[n]][ii])}
      # setting attributes
      for(ii in 1:nrow(diff)){
        diff[ii, paste0("s___",a,"_t", i_a)] =
          ifelse(
            diff[ii, n] != "" & ifelse(i_a > 1, !is.na(diff[ii, paste0("matched_t",i_a)]), T),
            paste0(
              "if(document.querySelector('[data-idd=\"",
              diff[ii, "data_id"],"\"]') != null){",
              "document.querySelector('[data-idd=\"",
              diff[ii, "data_id"],
              "\"]').setAttributeNS(null,
              '",a,"','",diff[ii, n],"'); ",
              "} ",""),
            ifelse(
              ifelse(i_a > 1, !is.na(diff[ii, paste0("matched_t",i_a)]), T) &
                a != "href", # not removing internal links
              paste0(
                "if(document.querySelector('[data-idd=\"",
                diff[ii, "data_id"],"\"]') != null){",
                "document.querySelector('[data-idd=\"",
                diff[ii, "data_id"],
                "\"]').removeAttributeNS(null,
              '",a,"'); ",
                "} ",
                ""),
              "" # do nothing
            )
          )
      }
    }
  }

  # collecting style changes
  for(i_a in 1:(length(svgs))){
    diff[paste0("s___style_t", i_a)] = "" # setting up variable to store changes
    # collecting short names
    n = paste0("attr___style___t", i_a)
    if(!n %in% names(diff)){diff[n] = ""}
    # setting na to empty string
    for(ii in 1:nrow(diff)){diff[[n]][ii] = ifelse(is.na(diff[[n]][ii]), "", diff[[n]][ii])}

    # setting style
    for(ii in 1:nrow(diff)){
      diff[ii, paste0("s___style_t", i_a)] =
        paste0(
          "if(document.querySelector('[data-idd=\"",diff[ii, "data_id"],"\"]') != null){",
          "document.querySelector('[data-idd=\"",diff[ii, "data_id"],"\"]').style='",diff[ii, n],"'; ",
          "}"
        )
    }
  }

  # collecting changes
  changes = sapply(
    1:length(svgs),
    function(x){
      # selecting rows that are not placeholders and columns that start with "s___" and contains the appropriate timing
      paste0(sapply(
        diff[!grepl(placeholder, diff[["data_id"]]), grepl("^s___", names(diff)) &
               grepl(paste0("_t", x,"$"),
                     names(diff))],
        function(y){ifelse(!is.na(y), y, "")}
      ), collapse = "")
    }
  )

  # removing redundant whitespace and new lines
  changes = gsub("(\\n|\\s\\s+)", "", changes)

  # getting svg to output
  svg_out = gsub("\\t", "", paste0(diff$svg___t1[!is.na(diff$svg___t1)], collapse = ""))

  # printing
  out = NULL
  ## html and svg
  out[1] = gsub("\\n", "", paste0("
    <style> #div_",img_id," * {transition: all ",
                                  animation_duration,
                                  " ",
                                  animation_curve,
                                  " !important;}
    #",img_id," {height: ",height," !important;}
    </style>
    <div id = 'div_",img_id,"'>",
                                  svg_out,
                                  "</div>",
                                  paste0("<div id = 'div_fragment_", img_id, "_", 0, "' ", "
           class='non-animated-fragment'> </div>"),
                                  paste(
                                    collapse = "",
                                    lapply(1:(length(svgs) - 1), # one less (+ 1 less because of js vs R convention)
                                           function(x){
                                             paste0("<div id = 'div_fragment_", img_id, "_", x, "' ", " class='fragment'> </div>"
                                             )})
                                  )))

  # detecting presence of fragments
  out[2] = gsub("\\n", "", paste0(
    "<script>
    var animate_frames_", img_id, " = ", length(svgs) - 1, ";
    var animate_count_", img_id, " = 0;
    console.log('animate_count');
    const changes_functions_", img_id," = [",
    paste0(
      lapply(0:(length(svgs)-1), function(x){
        paste0("function(){", changes[x+1], "}")}),
      collapse = ", "),
    "];
    const svg_img_", img_id," = document.querySelector('#", img_id,"');",
    "changes_functions_", img_id,"[0]();
    console.log('setting initial state');
    ",
    paste0(lapply(1:(length(svgs) - 1), function(x){
      paste0(
        "const div_fragment_", img_id,"_",x," =
        document.querySelector('#div_fragment_", img_id, "_", x,"');
        vis_prev_",img_id,"_",x," = false;
        const observer_",img_id,"_",x," =
          new MutationObserver((mutations_",img_id,"_",x,") =",">", "{
            mutations_",img_id,"_",x,".forEach((
              mutation_",img_id,"_",x,") =",">", "{
              if(
                vis_prev_",img_id,"_",x," == false &&
                animate_count_", img_id," < animate_frames_", img_id, "&&
                mutation_",img_id,"_",x,".type === 'attributes' &&
                mutation_",img_id,"_",x,".attributeName === 'class' &&
                mutation_",img_id,"_",x,".target ===
                  div_fragment_", img_id,"_",x," &&
                mutation_",img_id,"_",x,".target.classList.contains(
                  'current-fragment') &&
                mutation_",img_id,"_",x,".target.classList.contains(
                  'visible')) {
                    animate_count_", img_id,"=",x,";
                    changes_functions_",img_id,"[animate_count_", img_id,"]();
                    console.log('setting to frame: ');
                    console.log(animate_count_", img_id,");
                vis_prev_",img_id,"_",x," = true;

                  }
              if(
                vis_prev_",img_id,"_",x," == true &&
                animate_count_", img_id," > 0 &&
                mutation_",img_id,"_",x,".type === 'attributes' &&
                mutation_",img_id,"_",x,".attributeName === 'class' &&
                mutation_",img_id,"_",x,".target ===
                  div_fragment_", img_id,"_",x," &&
                !(mutation_",img_id,"_",x,".target.classList.contains(
                  'visible')) &&
                !(mutation_",img_id,"_",x,".target.classList.contains(
                  'visible'))) {
                    animate_count_", img_id,"=",x,"-1;
                    changes_functions_",img_id,"[animate_count_", img_id,"]();
                    console.log('setting to frame: ');
                    console.log(animate_count_", img_id,");
                vis_prev_",img_id,"_",x," = false;
                  }

                  });
                });
        observer_",img_id,"_",x,".observe(
          div_fragment_", img_id,"_",x,", { attributes: true });"
      )}), collapse = ""),
    paste0(" "),
    "</script>"))

  ## adding click
  out[3] = if(click){
    paste0(
      "<script>
          var click_direction_", img_id," = 1;
          function animate_click_", img_id, "() {
            console.log('click');
            if(click_direction_", img_id, " == 1 &&
              animate_count_", img_id, " < animate_frames_", img_id,") {",
      paste0("changes_functions_",img_id,
             "[animate_count_", img_id,"]();"),
      "animate_count_", img_id, "++;
                changes_functions_",img_id,"[animate_count_", img_id,"]();
                if(animate_count_", img_id, " == animate_frames_",
      img_id,"){
                    click_direction_", img_id, " = 0;
                    return;
                }
              }
          if(click_direction_", img_id, " == 0 &&
            animate_count_", img_id, " > 0) {",
      paste0("changes_functions_",img_id,
             "[animate_count_", img_id,"]();"),
      "animate_count_", img_id, "--;
          changes_functions_",img_id,"[animate_count_", img_id,"]();
          if(animate_count_", img_id, " == 0){
          click_direction_", img_id," = 1;
        }
      }
    }
    svg_img_", img_id,".addEventListener('click', animate_click_", img_id,");
    </script>", collapse = " ")

  } else {""}
  out[3] = gsub("\\n", "", out[3]) # removing new lines

  #binding everything together
  results = paste0(out, collapse = " ")
  attr(results, "html") = TRUE
  class(results) = c("html", "character")

  # adding whitespaces after "}"
  results = gsub("\\}", "} ", results)
  return(results)
}
