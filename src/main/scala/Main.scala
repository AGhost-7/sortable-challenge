

import play.api.libs.json._
import scala.annotation.switch
import scala.io.Source
import scala.collection.parallel.ParSeq
import java.util.concurrent.TimeUnit
import java.io.{File, FileWriter, BufferedWriter}

/** Model for listings.json objects. */
case class Listing(title: String, manufacturer: String, currency: String, price: String) {
	val manufacturerLower = manufacturer.toLowerCase

	override def toString =
		s"Title: $title, Manufacturer: $manufacturer, Currency: $currency, Price: $price"
}

/** Model for products.json objects. announced-date is omitted since it isn't
	* needed by the spec or the algorithm.
	*/
case class Product(name: String, manufacturer: String, family: Option[String], model: String) {
	val manufacturerLower = manufacturer.toLowerCase

	override def toString =
		s"Name: $name, Manufacturer: $manufacturer, Family: $family, Model: $model"
}

/** Since its part of the spec, performance needs to be measured. It seems that
	* Using the parallel collections works quite nicely. I'm running a quad-core
	* though, so it might be messing up the results a bit. Still probably worth it
	* even if you're running a dual core I think.
	*/
object Benchmarker {
	lazy val start = System.nanoTime

	def end = println("Milliseconds elapsed: " +
		(TimeUnit.NANOSECONDS.toMillis(System.nanoTime - start)))
}

object Main extends App {
	Benchmarker.start

	/** getLines returns a lazy iterator, so this is probably a pretty good way of
		* initalizing the processing.
		*/
	def loadJson[A](file: String)(mapper: JsValue => A): ParSeq[A] = {
		Source.fromFile(file).getLines.map { line =>
			mapper(Json.parse(line))
		}.toSeq.par
	}

	val products = loadJson("data/products.json") { js =>
		Product(
			(js \ "product_name").as[String],
			(js \ "manufacturer").as[String],
			(js \ "family").as[Option[String]],
			(js \ "model").as[String]
		)
	}

	val listings = loadJson("data/listings.json") { js =>
		Listing(
			(js \"title").as[String],
			(js \ "manufacturer").as[String],
			(js \ "currency").as[String],
			(js \ "price").as[String]
		)
	}

	def manufacturerMatch(listing: Listing, product: Product): Boolean = {
		listing.manufacturerLower.contains(product.manufacturerLower) ||
			product.manufacturerLower.contains(listing.manufacturerLower)
	}

	// "for",  probably not the way to go since there are multiple
	// languages here.
	val fors = List("für", "pour", "for")

	def isntAccessory(preModel: String, forIndex: Int = 0): Boolean =
		preModel.indexOf(fors(forIndex)) match {
			case -1 =>
				if(forIndex < fors.length - 1) isntAccessory(preModel, forIndex + 1)
				else true

			case indexFor => false

		}

	def modelMatch(listing: Listing, product: Product): Boolean = {
		val index = listing.title.indexOf(product.model)
		if(index > -1) {

			// Some "Special Edition", versions of the same model might exist, so I'd
			// better make sure this is a legit match. Also, 5000 contains 500, for
			// example...
			val postIndex = index + product.model.length
			if(postIndex != listing.title.length) {
				val postModel = listing.title.substring(postIndex)

				(postModel(0): @switch) match {
					case '\t' | '\r' | ' ' | ',' =>
						val preModel = listing.title.substring(0, index)
						isntAccessory(preModel)
					case _=> false
				}
			} else {
				true
			}
		} else {
			false
		}
	}

	def familyMatch(listing: Listing, product: Product): Option[Boolean] =
		product.family.map { fam => listing.title.contains(fam) }

	/** Used only for debugging. */
	def printMatches(product: Product, listings: Seq[Listing]): Unit = {
		val filtered = listings.filter { listing =>
			listing.title.toLowerCase.contains("case")
		}
		if(!filtered.isEmpty) {
			println("---------------------")
			println(product)
			println("---------------------")
			filtered.foreach(println)
		}

	}

	val matches = products.map { product =>
		val listingsMatches = listings.filter { listing =>
			manufacturerMatch(listing, product) &&
			modelMatch(listing, product) &&
				familyMatch(listing, product).getOrElse(true)
		}

	//	printMatches(product, listingsMatches)

		val jsonMatches = listingsMatches.map { listing =>
			Json.obj(
				"title" -> listing.title,
				"manufacturer" -> listing.manufacturer,
				"currency" -> listing.currency,
				"price" -> listing.price).toString
			//s"""{ "title": "${listing.title}", "manufacturer": "${listing.manufacturer}", "currency": "${listing.currency}", "price": "${listing.price}"}"""
		}.mkString(",")

		s"""{"product_name": "${product.name}", "listings": [$jsonMatches]"""
	}.seq


	val writer = new BufferedWriter(new FileWriter(new File("result.json")))

	matches.foreach { json => writer.write(json + "\n") }

	writer.close()

	Benchmarker.end

}
