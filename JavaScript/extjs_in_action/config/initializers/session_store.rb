# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key         => '_extjs_in_action_session',
  :secret      => 'fae4c2726c5806c82a9b9f0178e30edff637003680a8a643b82cc2b80310b39a88233468796e3430ea8a74759804a449cacf53f25fda6a28a203ff664100cbad'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
