Handlers.add("Ping",
   function (test) return true end,
   function(m)
       C = tonumber(m.Count)
       if C <= 3 then
           Send({ Target = ao.id, Action = "Ping", Count = C + 1 })
           print("Ping", C + 1)
       else
           print("Done.")
       end
   end
)
Send({ Target = ao.id, Action = "Ping", Count = 1 })