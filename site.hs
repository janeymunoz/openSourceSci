--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route   $ setExtension "html"
        compile $ do
            let pageCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    postCtx

            pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= loadAndApplyTemplate "templates/default.html" pageCtx
              >>= relativizeUrls

    match "about.md" $ do
        route   $ setExtension "html"
        compile $ do
            let pagesCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" "Open Source Science - About" `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" pagesCtx
                >>= relativizeUrls
    
    match "error.md" $ do
        route   $ setExtension "html"
        compile $ do
            let pagesCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" "Open Source Science - Error" `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" pagesCtx
                >>= relativizeUrls

    match "contact.html" $ do
        route idRoute
        compile $ do
            let pagesCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" "Open Source Science - Contact us" `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            getResourceBody
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" pagesCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    constField "title" "Open Source Science - The Archives"            `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    field "recent_posts" (\_ -> recentPostList) `mappend`
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Open Source Science" `mappend`
                    constField "site_desc" siteDesc          `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- Metadata
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "site_desc" siteDesc `mappend`
    defaultContext

siteDesc :: String
siteDesc = "The whole is greater than the sum of its parts"

--------------------------------------------------------------------------------
-- Recent Posts
recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*"
    return [Item identifier "" | identifier <- identifiers]

recentPostList :: Compiler String
recentPostList = do
    posts   <- fmap (take 10) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/listitem.html"
    list    <- applyTemplateList itemTpl defaultContext posts
    return list
