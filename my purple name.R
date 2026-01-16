# 基础图形系统实现
par(bg = "white", mar = rep(0,4))
plot(1, type = "n", xlim = c(0,1), ylim = c(0,1), axes = FALSE, ann = FALSE)

# 绘制主体结构
line_col <- "#6A0DAD" # 紫色
y_seq <- seq(0.3, 0.7, length.out = 7) # 垂直方向定位点

# 左侧垂直结构
segments(x0 = 0.35, y0 = y_seq, x1 = 0.4, y1 = y_seq, col = line_col, lwd = 3)
segments(x0 = 0.4, y0 = min(y_seq), x1 = 0.4, y1 = max(y_seq), col = line_col, lwd = 3)

# 右侧垂直结构（对称）
segments(x0 = 0.6, y0 = y_seq, x1 = 0.65, y1 = y_seq, col = line_col, lwd = 3)
segments(x0 = 0.6, y0 = min(y_seq), x1 = 0.6, y1 = max(y_seq), col = line_col, lwd = 3)

# 水平连接线
segments(x0 = 0.4, y0 = 0.3, x1 = 0.6, y1 = 0.3, col = line_col, lwd = 3)
segments(x0 = 0.4, y0 = 0.7, x1 = 0.6, y1 = 0.7, col = line_col, lwd = 3)

# 添加文字
text(x = 0.5, y = c(0.6, 0.5, 0.4), labels = "C", 
     col = line_col, cex = 2, font = 2)

# 使用grid包实现的另一种方式（可选）
library(grid)
grid.newpage()
grid.rect(gp = gpar(fill = "white", col = NA))

# 定义紫色样式
purple_line <- gpar(col = "#6A0DAD", lwd = 3)

# 绘制左侧结构
grid.polyline(x = unit.c(rep(unit(0.35, "npc"), 7), 
                         rep(unit(0.4, "npc"), 7)),
              y = unit(rep(seq(0.3, 0.7, length.out = 7), 2), "npc"),
              id = rep(1:7, 2),
              gp = purple_line)

# 绘制右侧结构（镜像对称）
grid.polyline(x = unit.c(rep(unit(0.65, "npc"), 7), 
                         rep(unit(0.6, "npc"), 7)),
              y = unit(rep(seq(0.3, 0.7, length.out = 7), 2), "npc"),
              id = rep(1:7, 2),
              gp = purple_line)

# 添加连接线
grid.segments(x0 = 0.4, x1 = 0.6, y0 = 0.3, y1 = 0.3, gp = purple_line)
grid.segments(x0 = 0.4, x1 = 0.6, y0 = 0.7, y1 = 0.7, gp = purple_line)

# 添加文字
grid.text(label = "胡\n志\n文", x = 0.5, y = 0.5, 
          gp = gpar(col = "#6A0DAD", fontsize = 24, fontface = "bold"))