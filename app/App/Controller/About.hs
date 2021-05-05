{-# LANGUAGE OverloadedStrings #-}

module App.Controller.About where

import App.Model.Env
import App.View.Page
import Data.Text
import Network.HTTP.Types
import Network.Wai.Responder
import Text.Blaze.Html (toHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

getPrivacyPolicy = do
  sendHtmlPage status200
    $ withLocation (TitledLocation "Privacy Policy")
    $ withHtml
    $ do
      H.h1 "Privacy Policy"
      H.p $ do
        H.strong "We do not share or sell your private information, and work hard to protect it."
        toHtml (" Your email address and followings are kept private. We do not collect any personal information besides the information you provide us through web forms." :: Text)
      H.p "This privacy policy is effective November 1st, 2020 and may be updated in the future."

getTerms = do
  env <- getEnv
  let displayUrl = renderDisplayUrl (appUrl env)
      displayName = appName env
      displayEmail = renderEmail (appEmail env)
      privacyPolicyUrl = appUrl env +> ["privacy"]
      legalName = displayName <> " Inc."
  sendHtmlPage status200
    $ withLocation (TitledLocation "Terms of Service")
    $ withHtml
    $ do
      H.h1 "Terms of Service"
      H.section $ do
        H.ul $ do
          H.li $ "We don't track you, or sell your information to third parties."
          H.li $ "You agree not to publish content that violates copyright; illegal content; content that threatens harm or violence; content considered spam."
          H.li $ toHtml $ "You will indemnify and hold harmless " <> displayName <> " for any and all damages resulting from the use of " <> displayUrl <> "."
          H.li $ toHtml $ "We may delete any content or revoke access to " <> displayUrl <> " for any reason, at any time."
          H.li $ toHtml $ "Children are not allowed to use " <> displayUrl <> ", or people whose use of " <> displayUrl <> " violates their country's law."
      H.section $ do
        H.p $ toHtml $
          "The following terms and conditions ('Terms') govern all use of the " <> displayUrl <> " website and all content, services, and products available at or through the website (taken together, our 'Services'). Our Services are offered subject to your acceptance without modification of all of the terms and conditions contained herein and all other operating rules, policies (including, without limitation, " <> displayUrl <> "'s Privacy Policy) and procedures that may be published from time to time by " <> displayUrl <> " (collectively, the 'Agreement'). You agree that we may automatically upgrade our Services, and these Terms will apply to any upgrades."
        H.p $ toHtml $ "We refer to " <> legalName <> " as '" <> displayName <> "' or 'we' throughout this Agreement."
        H.p $ "Please read this Agreement carefully before accessing or using our Services. By accessing or using any part of our Services, you agree to become bound by the Terms of this Agreement. If you do not agree to all the Terms of this Agreement, then you may not access or use any of our Services."
        H.p "Our Services are not directed to children. Access to and use of our Services is only for those over the age of 18. If you are younger than this, you may not register for or use our Services. Any person who registers as a user or provides their personal information to our Services represents that they are 18 years of age or older."
        H.h2 $ "Privacy Policy"
        H.p $ do
          toHtml ("Our privacy policy is published at " :: Text)
          H.a ! A.href (urlValue privacyPolicyUrl) $ toHtml $ renderDisplayUrl privacyPolicyUrl
          toHtml ("." :: Text)
        H.h2 "Responsibility of Users"
        H.p $ toHtml $
          "If you create a blog or account on " <> displayUrl <> ", you are responsible for maintaining the security of your blog or account, and fully responsible for all activities that occur under this account and any other actions taken in connection with the blog."
        H.p $ toHtml $
          "If you operate a blog, post material to " <> displayUrl <> ", post links to " <> displayUrl <> ", or otherwise make material available (any such material 'Content'), you are entirely responsible for the content of, and any harm resulting from, that Content or your conduct. That is the case regardless of what form the Content takes, which includes but is not limited to text, photo, video, audio, or code."
        H.p "By making Content available, you represent and warrant that:"
        H.ul $ do
          H.li "The use of the Content will not infringe the proprietary rights, including copyright, patent, or trademark rights, of any third party;"
          H.li "you have fully complied with any third-party licenses related to the Content;"
          H.li "the Content does not contain or install any malware, viruses, worms, or other harmful or destructive content;"
          H.li "the Content is not spam, and does not contain unwanted or unethical commercial content designed to drive traffic to third party websites or boost search engine rankings of third party websites, or to further unlawful acts (such as phishing) or mislead recipients as to the source of the material (such as spoofing);"
          H.li "the Content does not contain threats or incite violence towards individuals or entities, and does not violate the privacy or publicity rights of any third party;"
          H.li "your blog is not advertised via unwanted electronic messages such as spam links and similar unsolicited promotional methods;"
          H.li "your blog is not named in a manner that misleads your readers into thinking you are another person or company. For example, your blog's URL or name is not the name of a person other than yourself or company other than your own;"
          H.li $ toHtml $ "By submitting Content to " <> displayName <> ", you grant " <> displayName <> " a world-wide, royalty-free, and non-exclusive license to reproduce, modify, adapt, and publish the Content solely for the purposes of displaying, distributing, and promoting your blog;"
          H.li $ toHtml $ "If you delete Content, " <> displayName <> " will use reasonable efforts to remove it from " <> displayUrl <> ", but you acknowledge that caching or references to the Content may not be made unavailable."
        H.h2 "Responsibility of Visitors"
        H.p "We have not reviewed, and cannot review, all of the material (such as content) posted to our Services by users or anyone else ('Site Materials'), and are not responsible for any Site Materials' content, use, or effects. We do not endorse any Site Materials or represent that Site Materials are accurate, useful, or non-harmful. We also disclaim any responsibility for any harm resulting from anyone's use, purchase or downloading of Site Materials. If you access or use any Site Materials, you are responsible for taking precautions as necessary to protect yourself and your computer systems from viruses, worms, Trojan horses, and other harmful or destructive content. Site Materials may be offensive, indecent, objectionable, or include technical inaccuracies, typographical mistakes, and other errors, or violate or infringe the privacy or publicity rights, intellectual property rights, and other proprietary rights, of third parties. We are not a party to, and will have no responsibility or liability for, any communications, transactions, interactions, or disputes, between you and the provider of any Site Materials. Please note that additional terms and conditions may apply to the downloading, copying, or use of Site Materials."
        H.h2 "Copyright Infringement"
        H.p $ toHtml $ "As " <> displayName <> " asks others to respect its intellectual property rights, it respects the intellectual property rights of others. If you believe that material located on or linked to by " <> displayUrl <> " violates your copyright, you are encouraged to notify " <> displayName <> " at the contact information below. " <> displayName <> " will respond to all such notices, including as required or appropriate by removing the infringing material or disabling all links to the infringing material. " <> displayName <> " will terminate a visitor's access to and use of the website if, under appropriate circumstances, the visitor is determined to be a repeat infringer of the copyrights or other intellectual property rights of " <> displayName <> " or others. In the case of such termination, " <> displayName <> " will have no obligation to provide a refund of any amounts previously paid to " <> displayName <> "."
        H.h2 "Indemnification"
        H.p $ toHtml $ "You agree to indemnify and hold harmless " <> displayName <> ", its contractors, and its licensors, and their respective directors, officers, employees, and agents from and against any and all losses, liabilities, demands, damages, costs, claims and expenses, including attorneys' fees, arising out of or related to your use of our Services, including but not limited to your violation of this Agreement, and materials (such as content) your post."
        H.h2 "Termination"
        H.p $ toHtml $ displayName <> " may terminate your access to all or part of our Services at any time, with or without cause, with or without notice, effective immediately. If you wish to terminate this Agreement or your " <> displayName <> " account, you may simply discontinue using our Services."
        H.h2 "Questions"
        H.p $ toHtml $ "Any questions about these Terms of Service should be addressed to " <> displayEmail <> "."
