package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import libs.oauth.ConsumerKey
import libs.oauth.OAuth
import libs.oauth.OAuthCalculator
import libs.oauth.RequestToken
import libs.oauth.ServiceInfo
import libs.json.{Json, JsArray, JsValue, JsObject}
import libs.ws.WS
import models.TweetModel
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.net.{URLEncoder, URLDecoder}
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.Date
import java.util.Random


object Application extends Controller {
  val config = Play.current.configuration
  val consumerKey = config.getString("consumerKey").getOrElse("")
  val consumerSecret = config.getString("consumerSecret").getOrElse("")
  val accessToken = config.getString("accessToken").getOrElse("")
  val accessTokenSecret = config.getString("accessTokenSecret").getOrElse("")
  val myScreenName = config.getString("screenName").getOrElse("")
  val rand = new Random(System.currentTimeMillis())

  val key = ConsumerKey(consumerKey, consumerSecret)
  val twitterOAuth = OAuth(
    ServiceInfo(
      "https://api.twitter.com/oauth/request_token",
      "https://api.twitter.com/oauth/access_token",
      "https://api.twitter.com/oauth/authorize",
      key),
    false)
  val token = RequestToken(accessToken, accessTokenSecret)
  val oAuthCalculator = OAuthCalculator(key, token)
  val someDuration = Duration(10000, "millis")


  def index = Action {
    Redirect(routes.Application.timeline)
  }

  def timeline = Action { request => {
    val dateBeforeFormatter = new SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy",Locale.ENGLISH)
    val dateAfterFormatter = new SimpleDateFormat("yyyy-MM-dd",Locale.ENGLISH)
    val currentTime = new Date()
    val timeLine = getHomeTimeLine(oAuthCalculator)
    val tweets = timeLine.map {
      tweet => {
        val name = (tweet \ "user" \ "name").toString.replaceAll("\"", "")
        val screenName = (tweet \ "user" \ "screen_name").toString.replaceAll("\"", "")
        val createdAt = (tweet \ "created_at").toString.replaceAll("\"", "")
        var dateTime = diffTime(dateBeforeFormatter.parse(createdAt), currentTime)
        val text = (tweet \ "text").toString.replaceAll("\"", "")
        val imageUrl = (tweet \ "user" \ "profile_image_url").toString.replaceAll("\"", "")
        TweetModel(text, dateTime, screenName, name, imageUrl)
      }
    } .toArray
    Ok(views.html.timeline("Obaka Twitter Client", tweets))
  }}

  def post = Action { implicit request => {
    val comments = Array("hoge", "fuga", "piyo")
    val randomIndex = rand.nextInt(comments.length)
    val form = Form("message" -> nonEmptyText)
    val formMessage = form.bindFromRequest.fold(
      errors => throw new IllegalArgumentException("cannot post message"),
      message => message
      )
    val userData = getUsersShow(oAuthCalculator, myScreenName)
    val postCount = (userData \ "statuses_count").toString.toInt
    val message = if (postCount % 3 == 2) comments(randomIndex)+(postCount+1).toString else formMessage
    val userTimeline = postStatusUpdate(oAuthCalculator, message)
    Redirect(routes.Application.timeline)
  }}

  def getHomeTimeLine(oAuthCalculator: OAuthCalculator): Seq[JsValue] = {
    val url = "https://api.twitter.com/1.1/statuses/home_timeline.json"
    val resultPromise = WS.url(url).sign(oAuthCalculator).get
    Await.result(resultPromise, someDuration).json match {
      case JsArray(elem) => elem
      case _ => List.empty
    }
  }

  def postStatusUpdate(oAuthCalculator: OAuthCalculator, message: String): JsValue = {
    val url = "https://api.twitter.com/1.1/statuses/update.json?status="+URLEncoder.encode(message, "UTF-8")
    val resultPromise = WS.url(url).sign(oAuthCalculator).post("")
    Await.result(resultPromise, someDuration).json
  }

  def getUsersShow(oAuthCalculator: OAuthCalculator, screenName: String): JsValue = {
    val url = "https://api.twitter.com/1.1/users/show.json?screen_name="+screenName
    val resultPromise = WS.url(url).sign(oAuthCalculator).get
    Await.result(resultPromise, someDuration).json
  }

  def diffTime(createdAt:Date, current:Date): String = {
    val minuteUnit = 60 * 1000
    val diffMin = (current.getTime - createdAt.getTime) / minuteUnit
    if (diffMin < 60){
      diffMin.toString + " minutes"
    } else if (diffMin < 60 * 24) {
      (diffMin / 60) + " hours"
    } else {
      (diffMin / (60*24)) + " days"
    }
  }
}
