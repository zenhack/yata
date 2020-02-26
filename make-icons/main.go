package main

import (
	"flag"
	"image"
	"image/png"
	"log"
	"os"
)

var (
	size = flag.Int("size", 64, "Width/height of the generated image")
)

func chkfatal(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func main() {
	flag.Parse()
	srcImg, err := png.Decode(os.Stdin)
	chkfatal(err)

	srcBounds := srcImg.Bounds()
	srcWidth := srcBounds.Max.X - srcBounds.Min.X
	srcHeight := srcBounds.Max.Y - srcBounds.Min.Y

	if srcWidth != srcHeight {
		log.Fatal("Source image is not a square")
	}

	scaleFactor := *size / srcWidth

	dstImg := image.NewRGBA(image.Rectangle{
		Max: image.Point{
			X: *size,
			Y: *size,
		},
	})

	for x := 0; x < *size; x++ {
		for y := 0; y < *size; y++ {
			srcX := x / scaleFactor
			srcY := y / scaleFactor
			color := srcImg.At(srcX, srcY)
			dstImg.Set(x, y, color)
		}
	}
	chkfatal(png.Encode(os.Stdout, dstImg))
}
