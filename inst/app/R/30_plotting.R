# Path-diagram formatting helpers.

format_path_labels <- function(p) {
  tryCatch({
    if (is.null(p) || is.null(p$graphAttributes) || is.null(p$graphAttributes$Edges)) {
      return(p)
    }

    edges <- p$graphAttributes$Edges

    # 尝试修改 labelText 属性（qgraph 实际使用的标签）
    if (!is.null(edges$labelText)) {
      for (i in seq_along(edges$labelText)) {
        current <- edges$labelText[[i]]
        if (!is.null(current)) {
          # 尝试转换并格式化
          if (is.character(current)) {
            current <- current[length(current)]  # 取最后一个
          }
          num_val <- suppressWarnings(as.numeric(current))
          if (!is.na(num_val) && !is.nan(num_val)) {
            p$graphAttributes$Edges$labelText[[i]] <- sprintf("%.3f", num_val)
          }
        }
      }
    }

    # 如果 labelText 不存在或为空，尝试修改 label 属性
    if (is.null(edges$labelText) || all(sapply(edges$labelText, is.null))) {
      if (!is.null(edges$label)) {
        if (is.list(edges$label)) {
          for (i in seq_along(edges$label)) {
            if (!is.null(edges$label[[i]]) && is.character(edges$label[[i]])) {
              num_val <- suppressWarnings(as.numeric(edges$label[[i]][1]))
              if (!is.na(num_val) && !is.nan(num_val)) {
                p$graphAttributes$Edges$label[[i]] <- sprintf("%.3f", num_val)
              }
            }
          }
        } else if (is.character(edges$label)) {
          for (i in seq_along(edges$label)) {
            if (!is.na(edges$label[i]) && edges$label[i] != "") {
              num_val <- suppressWarnings(as.numeric(edges$label[i]))
              if (!is.na(num_val) && !is.nan(num_val)) {
                p$graphAttributes$Edges$label[i] <- sprintf("%.3f", num_val)
              }
            }
          }
        }
      }
    }

    # 强制更新 labelText 如果 label 被修改了
    if (!is.null(edges$label) && is.null(edges$labelText)) {
      if (is.list(edges$label)) {
        p$graphAttributes$Edges$labelText <- lapply(edges$label, function(x) {
          if (!is.null(x)) {
            num_val <- suppressWarnings(as.numeric(x[1]))
            if (!is.na(num_val) && !is.nan(num_val)) {
              return(sprintf("%.3f", num_val))
            }
          }
          return(x)
        })
      }
    }

    return(p)
  }, error = function(e) {
    # 如果出错了，返回原始对象
    return(p)
  })
}


# —— 自动布局（M 在上排；X/Y 在中下；Z 贴着 Y 的左上，仅用于 Y）——
make_layout_mediation_multi <- function(
  fit,
  xvars = NULL, mvars = NULL, yvars = NULL, zvars = NULL,
  patt_x="^X(\\d+)?$", patt_m="^M(\\d+)?$", patt_y="^Y(\\d+)?$", patt_z="^Z(\\d+)?$",
  y_m=0.72, y_xy=0.40, z_above_y_offset=0.30,   # Z 相对 Y 的竖直偏移
  z_left_dx=c(0.18, 0.06),                      # Z 相对 Y 的水平范围：左移 [0.18, 0.06]
  wiggle_m=TRUE
){
  m <- semPlot::semPlotModel(fit)
  labs <- tryCatch(m@Vars$name, error=function(e) m$Vars$name)
  lay <- matrix(NA_real_, nrow=length(labs), ncol=2,
                dimnames=list(labs, c("x","y")))
  clip01 <- function(v) pmin(pmax(v, 0.02), 0.98)

  xvars <- if (is.null(xvars)) character(0) else xvars
  mvars <- if (is.null(mvars)) character(0) else mvars
  yvars <- if (is.null(yvars)) character(0) else yvars
  zvars <- if (is.null(zvars)) character(0) else zvars

  Xs <- if (length(xvars) > 0) intersect(labs, xvars) else grep(patt_x, labs, value=TRUE)
  Ms <- if (length(mvars) > 0) intersect(labs, mvars) else grep(patt_m, labs, value=TRUE)
  Ys <- if (length(yvars) > 0) intersect(labs, yvars) else grep(patt_y, labs, value=TRUE)
  Zs <- if (length(zvars) > 0) intersect(labs, zvars) else grep(patt_z, labs, value=TRUE)

  # X：左列（中下）
  if (length(Xs)) {
    yx <- if (length(Xs)==1) y_xy else seq(y_xy-0.10, y_xy+0.10, length.out=length(Xs))
    lay[Xs,] <- cbind(rep(0.08, length(Xs)), clip01(yx))
  }

  # M*：上排（垂直分布，M1在上，M2在M1下面，M3在M2下面...）
  if (length(Ms)) {
    if (length(Ms) == 1) {
      xm <- 0.50
      ym <- y_m
    } else {
      # 多个M变量，按数字顺序从上到下分布
      xm <- rep(0.50, length(Ms))  # 水平位置居中

      # 垂直位置：M1在最上面，M2在M1下面，M3在M2下面...，间隔更大
      if (length(Ms) == 2) {
        # 2个M：上下分布，间隔更大
        ym <- c(y_m + 0.15, y_m - 0.15)
      } else if (length(Ms) == 3) {
        # 3个M：上中下分布，间隔更大
        ym <- c(y_m + 0.20, y_m, y_m - 0.20)
      } else if (length(Ms) == 4) {
        # 4个M：上下分布，每行2个，间隔更大
        ym <- c(y_m + 0.25, y_m + 0.08, y_m - 0.08, y_m - 0.25)
      } else if (length(Ms) == 5) {
        # 5个M：从上到下依次排列，间隔更大
        ym <- c(y_m + 0.30, y_m + 0.15, y_m, y_m - 0.15, y_m - 0.20)
      } else if (length(Ms) == 6) {
        # 6个M：从上到下依次排列，间隔更大
        ym <- c(y_m + 0.35, y_m + 0.20, y_m + 0.05, y_m - 0.05, y_m - 0.20, y_m - 0.35)
      } else {
        # 7个以上：使用更大的垂直范围，间隔更大
        ym <- seq(y_m + 0.40, y_m - 0.40, length.out=length(Ms))
      }
    }

    lay[Ms,] <- cbind(clip01(xm), clip01(ym))
  }

  # Y：右列（中下）
  if (length(Ys)) {
    yy <- if (length(Ys)==1) y_xy else seq(y_xy-0.10, y_xy+0.10, length.out=length(Ys))
    lay[Ys,] <- cbind(rep(0.92, length(Ys)), clip01(yy))
  }

  # Z*：垂直分布，放在Y的右上角，稍微高一点
  if (length(Zs) && length(Ys)) {
    # 获取Y的位置作为参考
    yx <- lay[Ys[1], 1]; yy <- lay[Ys[1], 2]

    if (length(Zs) == 1) {
      # 单个Z：放在Y的右上角，稍微高一点
      lay[Zs,] <- cbind(clip01(yx + 0.15), clip01(yy + 0.35))
    } else {
      # 多个Z：垂直分布，在Y的右上角，稍微高一点
      zx <- rep(yx + 0.15, length(Zs))  # 水平位置在Y的右边

      # 垂直位置：根据数量上下分布，整体调高
      if (length(Zs) == 2) {
        # 2个Z：上下分布
        zy <- c(yy + 0.40, yy + 0.30)
      } else if (length(Zs) == 3) {
        # 3个Z：上中下分布
        zy <- c(yy + 0.45, yy + 0.35, yy + 0.25)
      } else if (length(Zs) == 4) {
        # 4个Z：上下分布
        zy <- c(yy + 0.50, yy + 0.40, yy + 0.30, yy + 0.20)
      } else if (length(Zs) == 5) {
        # 5个Z：上中下分布
        zy <- c(yy + 0.55, yy + 0.45, yy + 0.35, yy + 0.25, yy + 0.15)
      } else {
        # 6个以上：使用更大的垂直范围，整体调高
        zy <- seq(yy + 0.60, yy + 0.15, length.out=length(Zs))
      }

      lay[Zs,] <- cbind(clip01(zx), clip01(zy))
    }
  }

  # 残差节点放到对应变量上方，增加垂直间距
  err_nodes <- grep("^(e\\.|ε\\.|E\\.)", rownames(lay), value=TRUE)
  if (length(err_nodes)) {
    for (e in err_nodes) {
      base <- sub("^(e\\.|ε\\.|E\\.)", "", e)
      if (base %in% rownames(lay)) {
        base_y <- lay[base, 2]
        base_x <- lay[base, 1]

        # 检查是否有Z变量在附近
        z_nearby <- FALSE
        if (length(Zs) > 0) {
          for (z in Zs) {
            if (z %in% rownames(lay)) {
              z_pos <- lay[z,]
              if (abs(z_pos[1] - base_x) < 0.20 && abs(z_pos[2] - base_y) < 0.20) {
                z_nearby <- TRUE
                break
              }
            }
          }
        }

        # 根据是否有Z变量调整偏移
        y_offset <- if (z_nearby) 0.20 else 0.15
        lay[e,] <- clip01(c(base_x, base_y + y_offset))
      }
    }
  }

  # 其余未定位的节点放在最下方
  miss <- which(is.na(lay[,1]))
  if (length(miss)) {
    # 检查Z变量的最低位置
    z_bottom <- if (length(Zs) > 0) min(lay[Zs, 2], na.rm=TRUE) else 0.60
    safe_y <- max(0.10, z_bottom - 0.15)  # 确保在Z变量下方
    lay[miss,] <- cbind(seq(0.10, 0.90, length.out=length(miss)), rep(safe_y, length(miss)))
  }

  lay
}
build_main_effect_model <- function(yvar, xvar, zvars = character(0)) {
  parts <- c(paste0(yvar, " ~ ", xvar))
  if (length(zvars) > 0) {
    parts <- c(parts, paste0(yvar, " ~ ", zvars))
  }
  paste(parts, collapse = "\n")
}

build_sem_paths_plot <- function(fit_obj, layout_obj, include_errors) {
  semPlot::semPaths(
    fit_obj,
    layout = layout_obj,
    fixed = TRUE,
    whatLabels = "std",
    residuals = include_errors,
    intercepts = FALSE,
    nCharNodes = 0,
    sizeMan = 12,
    sizeLat = 12,
    label.cex = 1.2,
    style = "lisrel",
    shapeMan = "rectangle",
    shapeLat = "rectangle",
    node.width = 0.4,
    node.height = 0.25,
    edge.label.cex = 0.6,
    edge.label.color = "#C62828",
    edge.label.position = 0.6,
    edge.label.bg = "white",
    edge.color = "black",
    edge.label.margin = 0.02,
    edge.width = 1.2,
    asize = 1.5,
    curve = 0.2,
    curvePivot = TRUE,
    residScale = 3,
    # rescale = FALSE,
    color = list(man = "white", lat = "white"),
    border.width = 2,
    label.color = "blue",
    thresholds = FALSE,
    exoVar = FALSE,
    exoCov = FALSE,
    what = "paths",
    DoNotPlot = TRUE
  )
}

mark_residual_edges <- function(path_obj, edge_color = "blue") {
  node_names <- path_obj$graphAttributes$Nodes$names
  from_names <- node_names[path_obj$Edgelist$from]
  to_names <- node_names[path_obj$Edgelist$to]
  vars <- unique(na.omit(node_names))

  resid_idx <- which(!is.na(to_names) & to_names %in% vars &
                       (!from_names %in% vars | is.na(from_names)))
  if (length(resid_idx) == 0) return(path_obj)

  edge_colors <- path_obj$graphAttributes$Edges$color
  edge_colors[resid_idx] <- edge_color
  path_obj$graphAttributes$Edges$color <- edge_colors
  if (!is.null(path_obj$graphAttributes$Edges$label.color)) {
    path_obj$graphAttributes$Edges$label.color[resid_idx] <- edge_color
  }
  path_obj
}

mark_zm_edges <- function(path_obj, fit_obj, edge_color = "red") {
  pe <- lavaan::parameterEstimates(fit_obj)
  zm_edges <- pe[grepl("^zm\\d+_\\d+$", pe$label), ]
  if (nrow(zm_edges) == 0 || is.null(path_obj$graphAttributes$Nodes$names)) return(path_obj)

  edge_colors <- path_obj$graphAttributes$Edges$color
  node_names <- path_obj$graphAttributes$Nodes$names

  for (i in seq_len(nrow(zm_edges))) {
    from_idx <- which(node_names == zm_edges$rhs[i])
    to_idx <- which(node_names == zm_edges$lhs[i])
    if (length(from_idx) > 0 && length(to_idx) > 0) {
      edge_idx <- which(path_obj$Edgelist$from == from_idx & path_obj$Edgelist$to == to_idx)
      if (length(edge_idx) > 0) {
        edge_colors[edge_idx] <- edge_color
      }
    }
  }

  path_obj$graphAttributes$Edges$color <- edge_colors
  path_obj
}

draw_path_panel <- function(path_obj, panel_title) {
  path_obj <- format_path_labels(path_obj)
  plot(path_obj)
  title(panel_title, cex.main = 1.2, font.main = 2)
}
