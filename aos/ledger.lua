--- A simple ledger implementation for the `ledger-process@1.0` device.
--- It is a simple process that maintains a balance for each address.
Ledger = Ledger or {}

Handlers.add("balance",
    function(msg)
        local address = msg.Address or msg.From
        print(Ledger[address] or 0)
    end
)

Handlers.add("debit",
    function(msg)
        local address = msg.Address or msg.From
        local amount = msg.Amount or 0
        Ledger[address] = math.floor((Ledger[address] or 0) - amount)
        print(amount)
        return Ledger[address]
    end
)

Handlers.add("credit",
    function(msg)
        local address = msg.Sender
        local amount = msg.Amount
        Ledger[address] = math.floor((Ledger[address] or 0) + amount)
        return Ledger[address]
    end
)

print("Ledger initialized.")