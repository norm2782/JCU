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

  clear: ->
    @destroy()
