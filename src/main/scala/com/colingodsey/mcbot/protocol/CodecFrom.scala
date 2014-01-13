package com.colingodsey.mcbot.protocol

object CodecFromLocal {
	implicit class AnyExt(val x: Any) extends AnyVal {
		def aiof[T]: T = x.asInstanceOf[T]
	}
}

trait CodecFrom {
	import CodecFromLocal._

	implicit def packetDescFromFieldDesc[T <: Packet](
			implicit comp: PacketCompanion[T],
			dec: FieldCodec[T]): PacketCodec[T] = new PacketCodec[T] {

		def read(src: DataSource): T = src.read[T](dec)

		def write(obj: T, dest: DataDest): Unit =
			dest.write(obj)(dec)

		def packetID: Byte = comp.packetID
	}





	implicit def codecFrom0[T <: Product](f: => T) = new FieldCodec[T] {
		
		def read(src: DataSource): T = f

		def write(obj: T, dest: DataDest) {
		}
	}

	implicit def codecFrom1[T <: Product, A](f: (A) => T)(
			implicit aCodec: FieldCodec[A]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
		}
	}


	implicit def codecFrom2[T <: Product, A, B](f: (A, B) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
		}
	}


	implicit def codecFrom3[T <: Product, A, B, C](f: (A, B, C) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
		}
	}


	implicit def codecFrom4[T <: Product, A, B, C, D](f: (A, B, C, D) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
		}
	}


	implicit def codecFrom5[T <: Product, A, B, C, D, E](f: (A, B, C, D, E) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
		}
	}


	implicit def codecFrom6[T <: Product, A, B, C, D, E, F](f: (A, B, C, D, E, F) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
		}
	}


	implicit def codecFrom7[T <: Product, A, B, C, D, E, F, G](f: (A, B, C, D, E, F, G) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
		}
	}


	implicit def codecFrom8[T <: Product, A, B, C, D, E, F, G, H](f: (A, B, C, D, E, F, G, H) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
		}
	}


	implicit def codecFrom9[T <: Product, A, B, C, D, E, F, G, H, I](f: (A, B, C, D, E, F, G, H, I) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
		}
	}


	implicit def codecFrom10[T <: Product, A, B, C, D, E, F, G, H, I, J](f: (A, B, C, D, E, F, G, H, I, J) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
		}
	}


	implicit def codecFrom11[T <: Product, A, B, C, D, E, F, G, H, I, J, K](f: (A, B, C, D, E, F, G, H, I, J, K) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
		}
	}


	implicit def codecFrom12[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L](f: (A, B, C, D, E, F, G, H, I, J, K, L) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
		}
	}


	implicit def codecFrom13[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M](f: (A, B, C, D, E, F, G, H, I, J, K, L, M) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
		}
	}


	implicit def codecFrom14[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
		}
	}


	implicit def codecFrom15[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
		}
	}


	implicit def codecFrom16[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
		}
	}


	implicit def codecFrom17[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
		}
	}


	implicit def codecFrom18[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q], rCodec: FieldCodec[R]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src, rCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
			rCodec.write(obj.productElement(17).aiof, dest)
		}
	}


	implicit def codecFrom19[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q], rCodec: FieldCodec[R], sCodec: FieldCodec[S]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src, rCodec read src, sCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
			rCodec.write(obj.productElement(17).aiof, dest)
			sCodec.write(obj.productElement(18).aiof, dest)
		}
	}


	implicit def codecFrom20[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q], rCodec: FieldCodec[R], sCodec: FieldCodec[S], uCodec: FieldCodec[U]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src, rCodec read src, sCodec read src, uCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
			rCodec.write(obj.productElement(17).aiof, dest)
			sCodec.write(obj.productElement(18).aiof, dest)
			uCodec.write(obj.productElement(19).aiof, dest)
		}
	}


	implicit def codecFrom21[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q], rCodec: FieldCodec[R], sCodec: FieldCodec[S], uCodec: FieldCodec[U], vCodec: FieldCodec[V]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src, rCodec read src, sCodec read src, uCodec read src, vCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
			rCodec.write(obj.productElement(17).aiof, dest)
			sCodec.write(obj.productElement(18).aiof, dest)
			uCodec.write(obj.productElement(19).aiof, dest)
			vCodec.write(obj.productElement(20).aiof, dest)
		}
	}


	implicit def codecFrom22[T <: Product, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V, W](f: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, U, V, W) => T)(
			implicit aCodec: FieldCodec[A], bCodec: FieldCodec[B], cCodec: FieldCodec[C], dCodec: FieldCodec[D], eCodec: FieldCodec[E], fCodec: FieldCodec[F], gCodec: FieldCodec[G], hCodec: FieldCodec[H], iCodec: FieldCodec[I], jCodec: FieldCodec[J], kCodec: FieldCodec[K], lCodec: FieldCodec[L], mCodec: FieldCodec[M], nCodec: FieldCodec[N], oCodec: FieldCodec[O], pCodec: FieldCodec[P], qCodec: FieldCodec[Q], rCodec: FieldCodec[R], sCodec: FieldCodec[S], uCodec: FieldCodec[U], vCodec: FieldCodec[V], wCodec: FieldCodec[W]) = new FieldCodec[T] {
		
		def read(src: DataSource): T =
			f(aCodec read src, bCodec read src, cCodec read src, dCodec read src, eCodec read src, fCodec read src, gCodec read src, hCodec read src, iCodec read src, jCodec read src, kCodec read src, lCodec read src, mCodec read src, nCodec read src, oCodec read src, pCodec read src, qCodec read src, rCodec read src, sCodec read src, uCodec read src, vCodec read src, wCodec read src)

		def write(obj: T, dest: DataDest) {
			aCodec.write(obj.productElement(0).aiof, dest)
			bCodec.write(obj.productElement(1).aiof, dest)
			cCodec.write(obj.productElement(2).aiof, dest)
			dCodec.write(obj.productElement(3).aiof, dest)
			eCodec.write(obj.productElement(4).aiof, dest)
			fCodec.write(obj.productElement(5).aiof, dest)
			gCodec.write(obj.productElement(6).aiof, dest)
			hCodec.write(obj.productElement(7).aiof, dest)
			iCodec.write(obj.productElement(8).aiof, dest)
			jCodec.write(obj.productElement(9).aiof, dest)
			kCodec.write(obj.productElement(10).aiof, dest)
			lCodec.write(obj.productElement(11).aiof, dest)
			mCodec.write(obj.productElement(12).aiof, dest)
			nCodec.write(obj.productElement(13).aiof, dest)
			oCodec.write(obj.productElement(14).aiof, dest)
			pCodec.write(obj.productElement(15).aiof, dest)
			qCodec.write(obj.productElement(16).aiof, dest)
			rCodec.write(obj.productElement(17).aiof, dest)
			sCodec.write(obj.productElement(18).aiof, dest)
			uCodec.write(obj.productElement(19).aiof, dest)
			vCodec.write(obj.productElement(20).aiof, dest)
			wCodec.write(obj.productElement(21).aiof, dest)
		}
	}

}
