ActionController::Routing::Routes.draw do |map|
  map.root :controller => 'hello_world'
  map.say_hi '/say_hi', :controller => 'hello_world', :action => 'say_hi'
end
