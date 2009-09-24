ActionController::Routing::Routes.draw do |map|
  map.root :controller => "ajax"
  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'
end
