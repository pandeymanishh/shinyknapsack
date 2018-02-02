library(shiny)
library(shiny.semantic)
library(shinyjs)
ui <- function() {  
  shinyUI(    
  semanticPage(      
  shinyjs::useShinyjs(),      
  tags$head(tags$script(src="tablesort.js")),     
  title = "My page",
   suppressDependencies("bootstrap"),     
   p(),      
   h4("Table with the class 'ui red table'"),      
   HTML('<table class="ui red table"> 
   <thead>    <tr>
   <th>Food</th>           
   <th>Calories</th>            
   <th>Protein</th>           
   </tr><tbody>            
   <tr>            
   <td>Apples</td>           
   <td>200</td>            
   <td>0g</td>            
   </tr>           
   <tr>            
   <td>Orange</td>           
   <td>310</td>            
   <td>0g</td>            
   </tr>           
   </tbody>           
   </table>'),            
  
   h4("Table with the class 'ui sortable celled table'"),      
   HTML('<table class="ui sortable celled table">  
   <thead>    
   <tr><th>Food</th>            
   <th>Calories</th>            
   <th>Protein</th>            
   </tr><tbody>            
   <tr>           
   <td>Apples</td>           
   <td>200</td>            
   <td>0g</td>            
   </tr>            
   <tr>            
   <td>Orange</td>            
   <td>310</td>           
   <td>0g</td>           
   </tr>            
   </tbody>            
   </table>')    )  
   )}
   
server <- shinyServer(function(input, output) {
  runjs("$('table').tablesort();")
} )  
shinyApp(ui = ui(), server = server)
