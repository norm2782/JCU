class exports.RuleTreeNode extends Backbone.Model
  # Available attributes:
  # rule :: Rule
  # childRules :: BackBone.Collection

  initialize: ->
    @set {childRules: new Backbone.Collection()}

  hasRule: ->
    @get('rule')?

  addRule: ->
    @get('childRules').add (new RuleTreeNode())
    @change()

  isValid: ->
    return true # TODO: Remove
    if !@hasRule()
      return false
    # TODO: Make sure a Rule object is available here

    @get('rule').validate() && @get('childRules').all((x) -> x.isValid())

  clear: ->
    @destroy()
