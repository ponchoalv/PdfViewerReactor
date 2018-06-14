module EditField

open Fulma

let editableField props editing labelProp labelElement =  
        (match editing with 
            | true ->  Input.text props
            | false -> Field.label [Field.Label.IsNormal] 
                        [ Label.label labelProp labelElement ] ) 