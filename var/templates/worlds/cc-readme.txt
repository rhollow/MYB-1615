CalCentral has *deleted* the following files from var/templates/worlds:

#   deleted:    var/templates/worlds/course/math-course.json
#   deleted:    var/templates/worlds/research.json
#   deleted:    var/templates/worlds/research/research-project.json

If these show up again via OAE merge, they should be removed with git rm - 
otherwise they'll appear in the world creation panes, and we don't want them.