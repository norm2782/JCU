class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: Term
  # childTerms :: BackBone.Collection

  initialize: =>
    @set {childTerms: new Backbone.Collection()}

  hasTerm: =>
    @get('term')?

  addRule: =>
    @get('childTerms').add(new ProofTreeNode())
    @change()

  isValid: =>
    return true # TODO: Remove
    if !@hasTerm()
      return false
    # TODO: Make sure a Rule object is available here

    @get('term').validate() && @get('childTerms').all((x) -> x.isValid())

  clear: =>
    @destroy()

