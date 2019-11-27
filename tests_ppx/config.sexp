( ( ( Or
      ( (Rule (typnam userid)) ; userid is a fully abstract type
        (Rule (colnam userid))
      )
    )
    ( (serialize Userid.to_string)
      (deserialize Userid.from_string)
    )
  )
  ( ( Or
      ( (Rule (typnam cash_money)) ; for strings beginning with a $ and possibly needing to be trimmed
        (And ; there exists a column elsewhere also named salary, but it has a different type
          ( (Rule (typnam float))
            (Rule (colnam salary))
          )
        )
      )
    )
    ( (serialize "fun x -> String.(sub x 1 (length x - 1)) |> String.trim")
      (deserialize "fun x -> \"$\" ^ x")
    )
  )
)