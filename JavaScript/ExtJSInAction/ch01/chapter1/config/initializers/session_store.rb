# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key         => '_chapter1_session',
  :secret      => 'ca54c3ce053ee042f008924632765683f8712bdd1c768734a4b3f93f943d6759c0c044afc5667207401ca0420955cd925ee6eb214e249bcf0b17c723c3f985f1'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
