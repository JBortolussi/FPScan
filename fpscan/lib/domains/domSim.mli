module DomSim : functor (DomRel : Relational.Domain) ->
                  functor (S : Scenario.Scenario_sig) ->
                    Relational.Domain