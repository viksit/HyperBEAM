--- A simple ledger implementation for the `ledger-process@1.0` device.
--- It is a simple process that maintains a balance for each address.
Ledger = Ledger or {}

Handlers.add("balance",
    function(msg)
        local address = msg.address or msg.From
        print(Ledger[address] or 0)
    end
)

Handlers.add("debit",
    function(msg)
        local address = msg.address or msg.From
        local amount = msg.amount
        Ledger[address] = (Ledger[address] or 0) - amount
        return Ledger[address]
    end
)

Handlers.add("credit",
    function(msg)
        if msg.From == Owner then
            local address = msg.address or msg.From
            local amount = msg.amount
            Ledger[address] = (Ledger[address] or 0) + amount
            return Ledger[address]
        end
        return nil
    end
)

print("Ledger initialized.")