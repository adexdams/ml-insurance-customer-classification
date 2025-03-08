library("VennDiagram")


### Example 1
grid.newpage()
draw.single.venn(area=10)


### Example 2
grid.newpage()
draw.pairwise.venn(area1 = 10, area2 = 20, cross.area = 2)


### Example 3
grid.newpage()
draw.triple.venn(area1 = 10, area2 = 20, area3 = 15, n12 = 2, n23 = 3, n13 = 17, n123 = 2)


### Example 4
grid.newpage()
draw.triple.venn(area1 = 10, 
                 area2 = 20, 
                 area3 = 15, 
                 n12 = 2, 
                 n23 = 3, 
                 n13 = 7, 
                 n123 = 2,
                 col = "red",
                 fill = "#1b98e0")


### Example 5
grid.newpage()
draw.triple.venn(area1 = 10, 
                 area2 = 20, 
                 area3 = 15, 
                 n12 = 2, 
                 n23 = 3, 
                 n13 = 7, 
                 n123 = 2,
                 col = "red",
                 fill = "#1b98e0")



### Example 6
grid.newpage()
draw.triple.venn(area1 = 10, 
                 area2 = 20, 
                 area3 = 15, 
                 n12 = 2, 
                 n23 = 3, 
                 n13 = 7, 
                 n123 = 2,
                 col = "red",
                 fill = c("pink", "green", "orange"),
                 alpha = 1)



### Example 7
grid.newpage()
draw.triple.venn(area1 = 10, 
                 area2 = 20, 
                 area3 = 15, 
                 n12 = 2, 
                 n23 = 3, 
                 n13 = 7, 
                 n123 = 2,
                 col = "red",
                 fill = c("pink", "green", "orange"),
                 lty = "blank")


### Example 8
grid.newpage()
draw.triple.venn(area1 = 10, 
                 area2 = 20, 
                 area3 = 15, 
                 n12 = 2, 
                 n23 = 3, 
                 n13 = 7, 
                 n123 = 2,
                 col = "red",
                 fill = c("pink", "green", "orange"),
                 lty = "blank",
                 category = c("Group 1", "Group 2", "Group 3"))
