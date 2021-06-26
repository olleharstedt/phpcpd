<?php declare(strict_types=1);
/*
 * This file is part of PHP Copy/Paste Detector (PHPCPD).
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
namespace SebastianBergmann\PHPCPD\Detector\Strategy\SuffixTree;

use SplPriorityQueue;
use function assert;
use Exception;

/**
 * Base class for units.
 * <p>
 * 
 * We have observed that in large code bases, the number of different units
 * after normalization is significantly smaller than the total number of units.
 * We exploit this observation in order to reduce the memory footprint of the
 * units by pooling unit content strings. This way, each unit content string is
 * only kept in memory once, independent of how often it occurs in the source
 * code. Since Java's {@link String#intern()} facility is too slow for very
 * large String pools, we use the {@link ConQATStringPool} in the constructor
 * {@link Unit}.
 * <p>
 * <b>Note:</b> The implementation of {@link #hashCode()} and
 * {@link #equals(Object)} are crucial for the detection algorithm.
 * 
 * @author $Author: kinnen $
 * @version $Rev: 49944 $
 * @ConQAT.Rating GREEN Hash: A696F6C00D704EEC8BBCB5753A3C98A6
 */
abstract class Unit
{

    /** The uniform path of the element this unit stems from.
     * @var string */
	private $elementUniformPath;

    /** The index of the unit in the element
     * @var int */
	private $indexInElement;

    /** The content of this unit.
     * @var string */
	private $content;

    /** The content of this unit without normalization
        * @var string */
	private $unnormalizedContent;

    /** Position of first character in element
     * @var int */
	private $filteredStartOffset;

    /** Position of last character in element (inclusive)
     * @var int */
	private $filteredEndOffset;

	/** Create a new unit with identical normalized and unnormalized content */
    /*
    protected Unit(
        int filteredStartOffset,
        int filteredEndOffset,
        String elementUniformPath,
        String content,
        int indexInElement
    ) {
		this(filteredStartOffset, filteredEndOffset, elementUniformPath,
				content, content, indexInElement);
	}
     */

	/** Create new unit */
    protected function __construct(
        int $filteredStartOffset,
        int $filteredEndOffset,
        string $elementUniformPath,
        string $content,
        string $unnormalizedContent,
        int $indexInElement
    ) {
		$this->filteredStartOffset = $filteredStartOffset;
		$this->filteredEndOffset = $filteredEndOffset;
		$this->elementUniformPath = $elementUniformPath;
		$this->content = $content;
        // NB
		//$this->unnormalizedContent = $unnormalizedContent;
		$this->unnormalizedContent = $content;
		$this->indexInElement = $indexInElement;
	}

	/** Returns {@link #elementUniformPath}. */
    public function getElementUniformPath(): string
    {
		return $this->elementUniformPath;
	}

	/** Returns {@link #filteredStartOffset}. */
    public function getFilteredStartOffset(): int
    {
		return $this->filteredStartOffset;
	}

	/** Returns {@link #filteredEndOffset}. */
    public function getFilteredEndOffset(): int
    {
		return $this->filteredEndOffset;
	}

	/** Returns {@link #indexInElement}. */
    public function getIndexInElement(): int
    {
		return $this->indexInElement;
	}

	/**
	 * Sets {@link #indexInElement}. This must be changed, e.g., when filtering
	 * the units list.
	 */
    public function setIndexInElement(int $newIndex): void
    {
		$this->indexInElement = $newIndex;
	}

	/** Returns {@link #content}. */
    public function getContent(): string
    {
		return $this->content;
	}

	/** Returns {@link #unnormalizedContent}. */
	public function getUnnormalizedContent(): stringa {
		return $this->unnormalizedContent;
	}

	/**
	 * The hash code of a unit object is identical to the hash code of its
	 * content string.
	 * 
	 * @see #getContent()
	 */
    public function hashCode(): int
    {
		return crc32($this->content);
	}

	/** Two unit objects are equal if there content strings are equal. */
    public function equals(object $other): bool
    {
		// contrary to the myths on the net this is not slower than catching
		// the class cast exception
		if (!($other instanceof Unit)) {
			return false;
		}

		// we use equals(), which is fast for the same string (i.e. same
		// reference) but still can cope with different references (if there are
		// non-interned strings).
		return $this->content === $other->content;
	}

	/**
	 * Returns whether this unit is synthetic. Default implementation returns
	 * false. Override for synthetic units
	 */
    public function isSynthetic(): bool
    {
		return false;
	}

	/** Checks whether other unit stems from same element */
    public function inSameElement(Unit $other): bool
    {
		return $this->getElementUniformPath() === $other.getElementUniformPath();
	}
}

class Delta
{
    /** The first list of objects.
     * @var T[] $a */
    private $a;

    /** The second list of objects.
     * @var T[] $b */
    private $b;

    /** Equator used for comparing elements. */
    private $equator;

    /** Length of {@link #a}. */
    private $n = 0;

    /** Length of {@link #b}. */
    private $m = 0;

    /** The maximal possible difference between {@link #a} and {@link #b}. */
    private $max;

    /**
     * Maximal size of the delta produced. If the "real" delta would be larger,
     * a truncated delta will be created.
     */
    private $maxDeltaSize = 0;

	/**
	 * This array stores the position at which a string is changed. If it is
	 * positive, it indicates an addition (i.e. the position is for the
	 * second string). Otherwise it is a deletion (i.e. the (negated)
	 * position is for the first string). To allow storing a sign for
	 * position 0, all positions are incremented before (so this has to be
	 * compensated for).
     * @var int[]
	 */
	private $position = [];

	/**
	 * This array stores the characters which are added or deleted
	 * (interpretation depends on {@link #position}).
     * @param T[]
	 */
	private $t;

    /**
     * @template T
     * @param T[] $a
     * @param T[] $b
     */
    private function __construct(array $a, array $b, int $maxDeltaSize = 2, $equator = null)
    {
		$this->a = $a;
		$this->b = $b;
		$this->maxDeltaSize = $maxDeltaSize;
		$this->equator = $equator;

		$this->n = count($a);
		$this->m = count($b);
		$this->max = $this->n + $this->m;
		//$this->v = new int[max + 1][];
		//$this->from = new boolean[max + 1][];
		$this->v = array_fill(0, $this->max + 1, []);
		$this->from = array_fill(0, $this->max + 1, null);
	}


    /**
     * @param T[] $a
     * @param T[] $b
     * @return Delta<T>
     */
    public static function computeDeltaStatic(array $a, array $b): self
    {
        $delta = new Delta($a, $b);
        return $delta->computeDelta();
    }

    private function computeDelta(): self
    {
        return $this->constructDelta($this->calculateDeltaSize());
    }

    /**
     * Returns the size of the delta, i.e. the number of additions and
     * deletions.
     */
    public function getSize(): int
    {
        return count($this->position);
    }

    private static function withSize(int $size, int $n, int $m): Delta
    {
        $delta = new self([], []);
        $delta->n = $n;
        $delta->m = $m;
        $delta->position = array_fill(0, $size, 0);
        $delta->t = array_fill(0, $size, null);
        return $delta;
    }

    public function getPosition(int $i): int
    {
        return $this->position[$i];
    }

	/** Constructs the actual delta. */
	private function constructDelta(int $size): Delta
    {
		$d = $size;
		$k = -$size;
		while ($this->v[$size][$size + $k] < $this->n || $this->v[$size][$d + $k] - $k < $this->m) {
			++$k;
		}

		$delta = self::withSize($size, $this->n, $this->m);

		$difference = $this->n - $this->m;
		while ($d > 0) {
			if ($this->from[$d][$d + $k]) {
				++$k;
			} else {
				--$k;
			}
			--$d;

			$x = $this->v[$d][$d + $k];
			$y = $x - $k;

			$newDifference = $x - $y;
			if ($newDifference > $difference || $x >= $this->n) {
				$delta->position[$d] = $y + 1;
				$delta->t[$d] = $this->b[$y];
			} else {
				$delta->position[$d] = -$x - 1;
				$delta->t[$d] = $this->a[$x];
			}
			$difference = $newDifference;
		}
		return $delta;
	}

    /**
     * Calculates the size of the delta (i.e. the number of additions and
     * deletions. Additionally the {@link #v} and {@link #from} arrays are
     * filled.
     */
	private function calculateDeltaSize(): int
    {
		$size = -1;
		for ($d = 0; $size < 0 && $d <= $this->max; ++$d) {
			//$this->v[$d] = new int[2 * $d + 1];
			//$this->from[$d] = new boolean[2 * $d + 1];
			$this->v[$d] = array_fill(0, 2 * $d + 1, 0);
			$this->from[$d] = array_fill(0, 2 * $d + 1, null);

			$bestSum = -1;
			for ($k = -$d; $k <= $d; $k += 2) {
				$x = 0;
				if ($d > 0) {
					if ($k == -$d
							|| $k != $d
							&& $this->v[$d - 1][$d - 1 + $k - 1] < $this->v[$d - 1][$d - 1 + $k + 1]) {
						$x = $this->v[$d - 1][$d - 1 + $k + 1];
						$this->from[$d][$d + $k] = true;
					} else {
						$x = $this->v[$d - 1][$d - 1 + $k - 1] + 1;
						$this->from[$d][$d + $k] = false;
					}
				}
				$y = $x - $k;
				//while ($x < $this->n && $y < $this->m && $this->equator->equals($this->a[$x], $this->b[$y])) {
				while ($x < $this->n && $y < $this->m && $this->a[$x] === $this->b[$y]) {
					++$x;
					++$y;
				}
				$this->v[$d][$d + $k] = $x;
				if ($d >= $this->maxDeltaSize && $x <= $this->n && $y <= $this->m && $x + $y > $bestSum) {
					$bestSum = $x + $y;

					// truncate strings
					$this->n = min($x, $this->n);
					$this->m = min($y, $this->m);
				}
				if ($x >= $this->n && $y >= $this->m) {
					$size = $d;
				}
			}
		}
		return $size;
	}
}

class SimpleRegion
{
    /** Region start position (inclusive).
     * @var int */
	private $start;

    /** Region end position (inclusive).
     * @var int */
	private $end;

	/** Constructor. */
    public function __construct(int $start, int $end)
    {
		$this->start = $start;
		$this->end = $end;
	}

	/** Checks if the region contains a position */
    public function containsPosition(int $position): bool
    {
		return $this->start <= $position && $this->end >= $position;
	}

	/** Checks if two regions are overlapping */
    public function overlaps(SimpleRegion $r): bool
    {
		// Region with smaller start value performs overlap check
		if ($r->start < $this->start) {
			return $r->overlaps($this->this);
		}

		return $start <= $r->start && $this->end >= $r->start;
	}

	/** Checks if two regions are adjacent */
    public function adjacent(SimpleRegion $r): bool
    {
		// Region with smaller start value performs adjacency check
		if ($r->start < $this->start) {
			return $r->adjacent($this);
		}

		return $this->end + 1 === $r->start;
	}

	/**
	 * Gets the end position of the region. This may be less than start for an
	 * empty region (see also {@link #isEmpty()}).
	 */
    public function getEnd(): int
    {
		return $this->end;
	}

	/** Gets the start position of the region */
    public function getStart(): int
    {
		return $this->start;
	}

	/**
	 * Gets the length of the region. Empty regions have a length of 0.
	 */
    public function getLength(): int
    {
		if ($this->isEmpty()) {
			return 0;
		}
		return $this->end - $this->start + 1;
	}

	/** Returns whether this region is empty. */
    public function isEmpty(): bool
    {
		return $this->end < $this->start;
	}

    public function __toString(): string
    {
		return "[" + $this->start + "-" + $this->end + "]";
	}

	/** Compares regions by their start position */
    public function compareTo(SimpleRegion $other): int
    {
		return $this->start <=> $other->start;
	}

	/**
	 * Returns whether start and end of the region is the same as for this
	 * region.
	 */
    public function equalsStartEnd(SimpleRegion $other): bool
    {
		return $this->start == $other->start && $this->end == $other->end;
	}
}

final class Region extends SimpleRegion
{
	/** Name that is used if region is created without name */
	public static $UNKNOWN_ORIGIN = "Unknown origin";

	/**
	 * Origin of the region. Can be used to store information about who created
	 * the region.
     * @var string
	 */
	private $origin;

	/**
	 * Creates a region with an origin. An empty region can be denoted with and
	 * end position smaller than start.
	 * 
	 * @param int $start Start position of the region
	 * @param int $end End position of the region
	 * @param strgin $origin Region origin. (i.e. region producer)
	 */
    public function __construct(int $start, int $end, string $origin = null)
    {
        parent::__construct($start, $end);
		$this->origin = $origin;
	}

	/** Get origin. */
	public function getOrigin(): string {
		return $this->origin;
	}
}

interface ICloneClassConstraint
{
	/** Returns true, if constraint is satisfied, false if not */
	public function satisfied(CloneClass $cloneClass): bool;
}

abstract class ConstraintBase implements ICloneClassConstraint
{
    public function process(): self
    {
		return $this;
	}

	/**
	 * Template method that deriving classes can override to perform
	 * initialization
	 */
    protected function setup(): void
    {
		// Empty default implementation
	}
}

class CardinalityConstraint extends ConstraintBase
{
	/** Constant used to denote infinity */
	public static $INFINITY = -1;

	/** Maximal number of required clones */
	private $max = PHP_INT_MAX;

	/** Minimal number of required clones */
	private $min;

	//@AConQATParameter(name = "cardinality", minOccurrences = 1, maxOccurrences = 1, description = "Cardinality thresholds")
	public function setCardinality($min, $max): void
    {
		$this->min = $min;
		$this->max = $max;
	}

    public function satisfied(CloneClass $cloneClass): bool
    {
		$size = count($cloneClass->getClones());
		return $size >= $this->min && ($this->max === self::$INFINITY || $size <= $this->max);
	}

}

/**
 * List of clone class constraints.
 * 
 * @author juergens
 * @author $Author: poehlmann $
 * @version $Rev: 48262 $
 * @ConQAT.Rating YELLOW Hash: E2EBDFD5096A19B9AE8118B396F65619
 */
class ConstraintList
{
    /** Version used for serialization.
     * @var $serialVersionUID */
	private $serialVersionUID = 1;

    /** @var ConstraintBase[] */
    private $constraints = [];

	/** Returns true, if the cloneClass satisfies all constraints, false if not */
    public function allSatisfied(CloneClass $cloneClass): bool
    {
		foreach ($this->constraints as $constraint) {
			if (!$constraint->satisfied($cloneClass)) {
				return false;
			}
		}
		return true;
	}

	/** Returns true, if the cloneClass satisfies one constraint, false if not */
    public function oneSatisfied(CloneClass $cloneClass): bool
    {
		foreach ($this->constraints as $constraint) {
			if ($constraints->satisfied($cloneClass)) {
				return true;
			}
		}
		return false;
	}

    public function add(ConstraintBase $constraint): void
    {
        $this->constraints[] = $constraint;
    }
}

class IdProvider
{
    /** Counter used to keep track of used ids */
    private $idCounter = 0;

    /** Constructor */
    public function __construct(int $lowestFreeId = 0) {
        $this->idCounter = $lowestFreeId;
    }

    /** Returns next fresh id. */
    public function provideId(): int
    {
        return $this->idCounter++;
    }
}

/**
 * Base class for locations. Locations are immutable and thus return this at
 * deep cloning.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 43290 $
 * @ConQAT.Rating GREEN Hash: 82BEB1645BABD9B7DBA7D15166C05209
 */
class ElementLocation
{
	/** Version used for serialization. */
	private static $serialVersionUID = 1;

    /** The location (see {@link #getLocation()}).
        * @var string */
	private $location;

    /** The uniform path (see {@link #getUniformPath()}).
        * @var string */
	private $uniformPath;

	/** Constructor. */
    public function __construct (string $location, string $uniformPath)
    {
		$this->location = $location;
		$this->uniformPath = $uniformPath;
	}

	/**
	 * Get a string that identifies the location of the element, e.g. a file
	 * system path. This location is specific to the running analysis, i.e.
	 * depends on time and the concrete machine ConQAT is running on.
	 */
    public function getLocation(): string
    {
		return $this->location;
	}

	/**
	 * Returns the uniform path. This is an artificial path that uniquely
	 * defines a resource across machine boundaries. This should be used for
	 * persisted information.
	 */
    public function getUniformPath(): string
    {
		return $this->uniformPath;
	}

    public function deepClone(): self
    {
		return $this;
	}

	/**
	 * Returns a single line description of the location that is meaningful to
	 * the user.
	 */
    public function toLocationString(): string
    {
		return getUniformPath();
	}

    public function __toString(): string
    {
		return $this->toLocationString();
	}
}

/**
 * This class denotes a region of text in an element.
 * 
 * <b>Context:</b> Due to the way ConQAT deals with text, the class is a little
 * bit more complex than expected. First, in ConQAT all text is normalized to
 * use Unix style line endings (regardless of the line endings in the file).
 * Second, ConQAT may apply filters, i.e. the internal (filtered) text
 * representation may be different from the (raw) text in the file.
 * Additionally, a location is best described by character offsets into the
 * string, while a user typically expects line numbers. Conversion between all
 * these representations is easy, as long as ConQAT internal representation is
 * available. Without it, conversion is not possible.
 * 
 * <b>Rationale:</b> When findings are reported to a user, the raw offsets
 * and/or lines should be used, as these are more meaningful (visible in other
 * editors as well). Also for persisting in a report, the raw positions are
 * preferred, as the filtered ones depend on the ConQAT configuration, while raw
 * offsets are independent of filter configuration. When working with findings
 * within ConQAT, typically the filtered positions are needed, as most
 * processors also work on the filtered representation. However, in such a case
 * the corresponding element is typically available and thus conversion to the
 * filtered representation is easy.
 * 
 * <b>Implementation:</b> The finding (as well as the findings report) only
 * stores raw positions. While the offsets would be sufficient, we also store
 * line numbers to be able to provide meaningful user output. Filtered positions
 * are not stored, but are made available via utility methods in the resource
 * bundle. All fields are mandatory, i.e., it is not allowed to fill any
 * position entry with invalid data (contrary to the old CodeRegionLocation,
 * where -1 could be used to denote missing information).
 * 
 * @author $Author: goede $
 * @version $Rev: 41698 $
 * @ConQAT.Rating GREEN Hash: 17529D0E53A92D53C5A9E732B5BA55C8
 */
class TextRegionLocation extends ElementLocation
{

	/** Version used for serialization. */
	private static $serialVersionUID = 1;

	/**
	 * The absolute start position of the region in the (raw) text (zero based,
	 * inclusive).
	 */
	private $rawStartOffset = 0;

	/**
	 * The absolute end position in the (raw) text (zero based, inclusive).
	 */
	private $rawEndOffset = 0;

	/**
	 * The line corresponding to {@link #rawStartOffset} (one-based, inclusive).
	 */
	private $rawStartLine = 0;

	/**
	 * The line corresponding to {@link #rawEndOffset} (one-based, inclusive).
	 */
	private $rawEndLine = 0;

	/** Constructor. */
    public function __construct(
        string $location,
        string $uniformPath,
        int $rawStartOffset,
        int $rawEndOffset,
        int $rawStartLine,
        int $rawEndLine
    ) {
        parent::__construct($location, $uniformPath);

		assert($rawStartOffset <= $rawEndOffset, "Start offset may not be after end offset.");
		assert($rawStartLine <= $rawEndLine, "Start line may not be after end line.");

		$this->rawStartOffset = $rawStartOffset;
		$this->rawEndOffset = $rawEndOffset;
		$this->rawStartLine = $rawStartLine;
		$this->rawEndLine = $rawEndLine;
	}

	/**
	 * Returns the absolute start position of the region in the (raw) text (zero
	 * based, inclusive).
	 */
    public function getRawStartOffset(): int
    {
		return $this->rawStartOffset;
	}

	/**
	 * Returns the absolute end position in the (raw) text (zero based,
	 * inclusive).
	 */
    public function getRawEndOffset(): int
    {
		return $this->rawEndOffset;
	}

	/**
	 * Returns the line corresponding to {@link #getRawStartOffset()}
	 * (one-based, inclusive).
	 */
    public function getRawStartLine(): int
    {
		return $this->rawStartLine;
	}

	/**
	 * Returns the line corresponding to {@link #getRawEndOffset()} (one-based,
	 * inclusive).
	 */
    public function getRawEndLine(): int
    {
		return $this->rawEndLine;
	}

    public function getUnfilteredOffset(int $offset): int
    {
        return $offset;
    }

    public function convertUnfilteredOffsetToLine(int $offset): int
    {
        return $offset;
    }

	/**
	 * <p>
	 * This includes the start and end line which is typically sufficient for
	 * debugging and showing to a user.
	 */
    public function toLocationString(): string
    {
		return parent::toLocationString() . ":" . $this->rawStartLine . "-" . $this->rawEndLine;
	}
}

class CloneUtils
{
    /** Key under which list of units is stored */
    const UNIT_LIST = "unit_list";

    /**
     * @param CloneObject[]
     */
    private static function allFingerprintsEqual(array $clones): bool
    {
        /** @var string */
        $commonFingerprint = $clones[0]->getFingerprint();
        foreach ($clones as $clone) {
            if ($commonFingerprint !== $clone->getFingerprint()) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param CloneObject[]
     */
    public static function createFingerprint(array $clones): string
    {
        if (empty($clones)) {
            throw new Exception('Cannot create fingerprint for empty collection of clones');
        }

        if (self::allFingerprintsEqual($clones)) {
            return $clones[0]->getFingerprint();
        }

        // as long as the fingerprints of the individual clones do not change,
        // we need to assure that the clone class fingerprint does not change
        // either. in order to compute a stable fingerprint for a clone class,
        // we thus need to establish a stable order in which clone fingerprints
        // are added to the clone class fingerprint. since we want to be
        // independent of clone filenames or clone positions in files, we
        // compute this order directly on the fingerprints.
        $fingerprints = [];
        foreach ($clones as $clone) {
            $fingerprints[] = $clone->getFingerprint();
        }

        return md5(implode(' ', $fingerprints));
    }

    /** Store list of units at a clone
     * @param Unit[] */
    public static function setUnits(CloneObject $clone, array $cloneUnits): void
    {
        $clone->setValue(self::UNIT_LIST, $cloneUnits);
        $clone->setTransient(self::UNIT_LIST, true);
    }
}

abstract class KeyValueStoreBase
{
	/**
	 * Integer that identifies this value store. The scope of the id depends on
	 * the id provider used.
     * @var int
	 */
	protected $id;

    /** Map that stores key-value pairs
     * @var array<string, object> */
	private $values = [];

    /** Map that stores transient flags
     * @var array<string, bool> */
	private $transientFlags = [];

	/** Construct store with set id */
	protected function __construct(int $id) {
		$this->id = $id;
	}

	/** Return the id of the store */
    public function getId(): int
    {
		return $this->id;
	}

	/** Stores a value in the {@link CloneClass} by using a keyword. */
    public function setValue(string $key, object $value): void
    {
		$this->values[key] = $value;
	}

	/** Gets a value */
    public function getValue(string $key): ?object
    {
		if ($this->values === null) {
			return null;
		}
		return $this->values[$key];
	}

	/** Checks whether a value is stored under this key */
    public function containsValue(string $key): bool
    {
		if ($this->values === null) {
			return false;
		}
		return array_key_exists($key, $this->values);
	}

}

class CloneObject extends KeyValueStoreBase
{
    /** {@link CloneClass} this clone belongs to
     * @var CloneClass */
	private $cloneClass;

    /** The location of the clone.
     * @var TextRegionLocation */
	private $location;

    /** Fingerprint of clone
     * @var string */
	private $fingerprint;

    /** Position of the first unit of the clone in its element
     * @var int */
	private $startUnitIndexInElement;

    /** Length of the clone in units
     * @var int */
	private $lengthInUnits;

	/**
	 * The gaps in the clone stored as region of raw offsets (absolute in
	 * element).
     *
     * @var ?Region[]
	 */
	public $gaps = null;

    /** Delta size in units
     * @var int */
	protected $deltaInUnits;

	/** Creates a clone with a delta in units of 0 */
    /*
	public Clone(long id, CloneClass cloneClass, TextRegionLocation location,
			int startUnitIndexInElement, int lengthInUnits, String fingerprint) {
		this(id, cloneClass, location, startUnitIndexInElement, lengthInUnits,
				fingerprint, 0);
	}
     */

	/**
	 * @param CloneClass $cloneClass this may be null to explicitly create a clone instance without clone class.
	 */
    public function __construct(
        int $id,
        CloneClass $cloneClass,
        TextRegionLocation $location,
        int $startUnitIndexInElement,
        int $lengthInUnits,
        string $fingerprint,
        int $deltaInUnits
    ) {
        parent::__construct($id);

        if ($location === null) {
            throw new Exception('$location is null');
        }

		$this->cloneClass = $cloneClass;

		// We use the Java string pool here for because:
		// - during clone detection, many clones can be created
		// - all fingerprints of non-gapped clones in same clone class are equal
		// (but created as different instances)
		//$this->fingerprint = $fingerprint->intern();
		$this->fingerprint = $fingerprint;

		$this->location = $location;
		$this->startUnitIndexInElement = $startUnitIndexInElement;
		$this->lengthInUnits = $lengthInUnits;
		$this->deltaInUnits = $deltaInUnits;

        if (!($location->getRawEndOffset() >= $location->getRawStartOffset())) {
            throw new Exception("Length must not be negative: " . (string) $this);
        }

		if ($cloneClass !== null) {
			$cloneClass->add($this);
		}
	}

	/**
	 * Two clones are considered equal, if they describe the same region of code
	 * in the same element with the same gaps.
	 */
    public function equals(object $obj): bool
    {
		if (!(!$obj instanceof CloneObject)) {
			return false;
		}

		$other = $obj;

		// compare source
		if (!$location->getUniformPath()->equals($other->location->getUniformPath())) {
			return false;
		}

		// compare start units
		if ($this->getStartUnitIndexInElement() !== $other->getStartUnitIndexInElement()) {
			return false;
		}

		// compare length in units
		if ($this->getLastUnitInElement() != $other->getLastUnitInElement()) {
			return false;
		}

		// compare gaps
		if ($this->gapCount() !== $other->gapCount()) {
			return false;
		}

		for ($i = 0; $i < $this->gapCount(); ++$i) {
			if (!$this->gaps[$i]->equalsStartEnd($other->gaps[$i])) {
				return false;
			}
		}

		// if code reaches here, clones are equal
		return true;
	}

	/**
	 * Returns the hash code based on the uniform path, the start and length.
	 * Gap information is ignored.
	 */
    public function hashCode(): int
    {
		return ($location->getUniformPath()->hashCode() * 13 + $this->getStartUnitIndexInElement()) * 17 + $this->getLengthInUnits();
	}

	/** {@link CloneClass} this clone belongs to */
    public function getCloneClass(): CloneClass
    {
		return $this->cloneClass;
	}

	/**
	 * Fingerprint of clone. A clone fingerprint characterizes the piece of
	 * cloned code after normalization. All ungapped clones inside a single
	 * clone class have the same fingerprint.
	 */
    public function getFingerprint(): string
    {
		return $this->fingerprint;
	}

	/** Position of the first unit of the clone in its element */
    public function getStartUnitIndexInElement(): int
    {
		return $this->startUnitIndexInElement;
	}

	/** Length of the clone in units. */
    public function getLengthInUnits(): int
    {
		return $this->lengthInUnits;
	}

	/** Position of the last unit of the clone in its element */
    public function getLastUnitInElement(): int
    {
		return $this->getStartUnitIndexInElement() + $this->getLengthInUnits() - 1;
	}

	/** Edit distance to first clone in clone class */
    public function getDeltaInUnits(): int
    {
		return $this->deltaInUnits;
	}

	/** Returns the clone's location. */
    public function getLocation(): TextRegionLocation
    {
		return $this->location;
	}

	/** Returns the uniform path of this clone's location. */
    public function getUniformPath(): string
    {
		return $this->location->getUniformPath();
	}

	/** Determines whether the clone contains gaps */
    public function containsGaps(): bool
    {
		return $this->gaps !== null && count($this->gaps) > 0;
	}

	/** Return number of gaps, or 0, if clone has no gaps */
    public function gapCount(): int
    {
		if ($this->gaps === null) {
			return 0;
		}
		return count($this->gaps);
	}

	/**
	 * Returns a list of the gap positions as raw start end offsets (absolute
	 * offsets in the element), or empty list, if clone has no gaps.
     *
     * @return Region[]
	 */
	public function getGapPositions(): array {
		if ($this->gaps === null) {
			return [];
		}

        $list = $this->gaps;
        asort($list);
		return $list;
	}

	/** Adds a gap position. */
    public function addGap(Region $gapRegion): void
    {
		if ($this->gaps === null) {
			$this->gaps = [];
		}
		$this->gaps[] = $gapRegion;
	}

	/** Set delta size in units */
    public function setDeltaInUnits(int $deltaInUnits): void
    {
		$this->deltaInUnits = $deltaInUnits;
	}

	/** String representation of the essential clone data. */
    public function __toString(): string
    {
		return "Clone [" . $this->location->toLocationString() . "]";
	}

	/** Sets the clone class. Only called from CloneClass. */
    public function setCloneClass(CloneClass $cloneClass): void
    {
		$this->cloneClass = $cloneClass;
	}
}

class CloneClass extends KeyValueStoreBase
{

    /** The length of the clone class (number of units).
     * @var int */
	public $normalizedLength;

    /** A list containing all clones of a class.
     * Hash table and linked list implementation of the Set interface, with predictable iteration order. This implementation differs from HashSet in that it maintains a doubly-linked list running through all of its entries. This linked list defines the iteration ordering, which is the order in which elements were inserted into the set (insertion-order). Note that insertion order is not affected if an element is re-inserted into the set. (An element e is reinserted into a set s if s.add(e) is invoked when s.contains(e) would return true immediately prior to the invocation.) 
     * @var CloneObject[] */
	public $clones = [];

	/**
	 * Create a new clone class with a given id
	 * 
	 * @param int $normalizedLength Length of the clones (in units) in this clone class
	 */
    public function __construct(int $normalizedLength, int $id)
    {
        parent::__construct($id);
		$this->normalizedLength = $normalizedLength;
	}

	/**
	 * The normalized length of all clones in this class in units and thereby
	 * the length of this class.
	 */
    public function getNormalizedLength(): int
    {
		return $this->normalizedLength;
	}

	/** Set normalized length */
    public function setNormalizedLength(int $normalizedLength): void
    {
		$this->normalizedLength = $normalizedLength;
	}

	/** Computes and returns the fingerprint for this clone class */
    public function getFingerprint(): string
    {
		return CloneUtils::createFingerprint($this->getClones());
	}

    /** Returns the clones of this clone class.
        * @return CloneObject[] */
    public function getClones(): array
    {
		return $this->clones;
	}

	/** The size (number of clones) of this clone class. */
    public function size(): int
    {
		return count($this->clones);
	}

	/** Two clone classes are equal, if their fingerprints are equal */
    public function equals(object $obj): bool
    {
		if (!($obj instanceof CloneClass)) {
			return false;
		}

		return $this->getFingerprint() === $obj->getFingerprint();
	}

	/** Returns hash code of fingerprint */
    public function hashCode(): int
    {
		return crc32($this->getFingerprint());
	}

	/** Returns the number of gaps in the clone with the most gaps */
    public function getMaxGapNumber(): int
    {
		$gapCount = 0;
        foreach ($this->getClones() as $clone) {
			if ($gapCount < $clone->gapCount()) {
				$gapCount = $clone->gapCount();
			}
		}
		return $gapCount;
	}

	/** Returns sum of gaps contained in all clones */
    public function getGapCount(): int
    {
		$gapCount = 0;
        foreach ($this->getClones() as $clone) {
			$gapCount += $clone->gapCount();
		}
		return $gapCount;
	}

	/**
	 * Adds a clone to this class.
	 * <p>
	 * No sanity check is performed that makes sure that a clone really belongs
	 * to a clone class in order to allow different clone detection approaches
	 * to form clone classes for different notions of similarity
	 */
    public function add(CloneObject $clone): void
    {
        $cloneIsInOtherClass =
               $clone->getCloneClass()->id !== $this->id
            && $clone->getCloneClass() !== null;
		if ($cloneIsInOtherClass) {
			$clone->getCloneClass()->remove($clone);
		}

		$this->clones[] = $clone;
		$clone->setCloneClass($this);
	}

	/** Removes a clone */
    public function remove(CloneObject $clone): void
    {
		if ($this->removeClone($clone)) {
			$clone->setCloneClass(null);
		}
	}

    /**
     * @return bool Returns true if $cloneSearch was removed from clones.
     */
    public function removeClone($cloneSearch): bool
    {
        $key = $this->findKeyOfClone($cloneSearch);
        if ($key !== null) {
            unset($this->clones[$key]);
            return true;
        }
        return false;
    }

    public function findKeyOfClone(CloneObject $cloneSearch): ?int
    {
        foreach ($this->getClones() as $i => $clone) {
            if ($clone->equals($cloneSearch)) {
                return $i;
            }
        }
        return null;
    }
}

class MultiplexingCloneClassesCollection
{
    //private final List<Collection<CloneClass>> collections = new ArrayList<Collection<CloneClass>>();
    /** Underlying collections
     * @var SplPriorityQueue[]
     */
    private $collections = [];

    public function __construct()
    {
    }

    /** Adds a clone class to all collections */
    public function add(CloneClass $cloneClass): void
    {
        //for (Collection<CloneClass> collection : collections) {
        foreach ($this->collections as $collection) {
            $collection->insert($cloneClass, 1);
        }
    }

    /**
     * Returns a list with all clones in the contained collections. The list
     * is sorted by normalized length and contains no duplicates.
     * @return CloneClass[]
     */
    public function getCloneClasses(): array
    {
        /** @var CloneClass[] */
        $resultSet = [];

        foreach ($this->collections as $boundedCollection) {
            foreach ($boundedCollection as $item) {
                $resultSet[] = $item;
            }
        }

        //asort($resultSet);
        // TODO: Sort by length
        //return CollectionUtils.sort(resultSet, ECloneClassComparator.NORMALIZED_LENGTH);
        return $resultSet;
    }

    /** Add a collection */
    public function addCollection(SplPriorityQueue $collection): void
    {
        $this->collections[] = $collection;
    }
}

class CloneConsumer
{
    /** List in which the created clone classes are stored
     * @var MultiplexingCloneClassesCollection */
    private $results;

    /** @var IdProvider */
    private $idProvider;

    /** @var int */
    private $top = PHP_INT_MAX;

    // Testing with tokens
    /** @var Token[] */
    protected $units = [];

    /** @var array<string, TextRegionLocation> */
	private $uniformPathToElement = [];

    /**
     * List of constraints that all detected clone classes must satisfy
     * @var ConstraintList
     */
	private $constraints;

    /** {@link CloneClass} currently being filled
     * @var CloneClass */
    protected $currentCloneClass;

	/** Flag that determines whether the units are stored in clones */
	protected $storeUnits = false;

    /**
     * Creates a ICloneConsumer that writes the {@link CloneClass}es it
     * creates into the given set
     */
    public function __construct(array $word)
    {
        $this->results = new MultiplexingCloneClassesCollection();
        $this->idProvider = new IdProvider();
        $this->constraints = new ConstraintList();
        $this->constraints->add(new CardinalityConstraint());
        $this->units = $word;

        foreach ($word as $token) {
            $this->uniformPathToElement[$token->file . '_' . $token->line] = new TextRegionLocation(
                $token->file . '_' . $token->line,
                $token->file . '_' . $token->line,
                $token->line,
                $token->line,
                $token->line,
                $token->line,
            );
        }

        if ($this->top === PHP_INT_MAX) {
            $this->results->addCollection(new SplPriorityQueue());
        } else {
            //$this->results->addCollection($this->boundedCollection(NORMALIZED_LENGTH));
            //$this->results->addCollection($this->boundedCollection(CARDINALITY));
            //$this->results->addCollection($this->boundedCollection(VOLUME));
            $this->results->addCollection($this->boundedCollection());
            $this->results->addCollection($this->boundedCollection());
            $this->results->addCollection($this->boundedCollection());
        }
    }

    /** Creates {@link BoundedPriorityQueue}
        * @return CloneClass[] */
    //private function boundedCollection(ECloneClassComparator $dimension): array
    private function boundedCollection(): SplPriorityQueue
    {
        return new SplPriorityQueue();
        //return new BoundedPriorityQueue<CloneClass>(top, dimension);
    }

    /** Start new clone class */
    public function startCloneClass(int $normalizedLength): void
    {
        $this->currentCloneClass = new CloneClass($normalizedLength, $this->idProvider->provideId());
    }

    /** Adds a clone to the current {@link CloneClass} */
    public function addClone(int $globalPosition, int $length): CloneObject
    {
        //var_dump($globalPosition);
        //var_dump($length);
        // compute length of clone in lines
        $firstUnit = $this->units[$globalPosition];
        $lastUnit = $this->units[$globalPosition + $length - 1];
        /** @var Token[] */
        $cloneUnits = array_slice($this->units, $globalPosition, $globalPosition + $length);

        $element = $this->resolveElement($firstUnit->getElementUniformPath());
        //var_dump($element);
        $startUnitIndexInElement = $firstUnit->getIndexInElement();
        $endUnitIndexInElement = $lastUnit->getIndexInElement();
        $lengthInUnits = $endUnitIndexInElement - $startUnitIndexInElement + 1;
        //var_dump($lengthInUnits);
        assert($lengthInUnits >= 0, "Negative length in units!");
        $fingerprint = $this->createFingerprint($globalPosition, $length);
        //var_dump($fingerprint);
        //var_dump($this->currentCloneClass);

        $cloneObject = new CloneObject(
            $this->idProvider->provideId(),
            $this->currentCloneClass,
            $this->createCloneLocation(
                $element,
                $firstUnit->getFilteredStartOffset(),
                $lastUnit->getFilteredEndOffset()
            ),
            $startUnitIndexInElement,
            $lengthInUnits,
            $fingerprint,
            0  // deltaInUnits
        );

        if ($this->storeUnits) {
            CloneUtils::setUnits($cloneObject, $cloneUnits);
        }

        $this->currentCloneClass->add($cloneObject);

        return $cloneObject;
    }

    /** Creates the location for a cloneObject. */
    private function createCloneLocation(
        TextRegionLocation $element,
        int $filteredStartOffset,
        int $filteredEndOffset
    ): TextRegionLocation {
        $rawStartOffset = $element->getUnfilteredOffset($filteredStartOffset);
        $rawEndOffset   = $element->getUnfilteredOffset($filteredEndOffset);
        $rawStartLine   = $element->convertUnfilteredOffsetToLine($rawStartOffset);
        $rawEndLine     = $element->convertUnfilteredOffsetToLine($rawEndOffset);

        return new TextRegionLocation(
            $element->getLocation(),
            $element->getUniformPath(),
            $rawStartOffset,
            $rawEndOffset,
            $rawStartLine,
            $rawEndLine
        );
    }

    /** Determine element for unit */
    protected function resolveElement(string $elementUniformPath): TextRegionLocation
    {
        return $this->uniformPathToElement[$elementUniformPath];
    }

    /** Create fingerprint for current cloneObject */
    protected function createFingerprint(int $globalPosition, int $length): string
    {
        $fingerprintBase = '';
        for ($pos = $globalPosition; $pos < $globalPosition + $length; $pos++) {
            $fingerprintBase .= $this->units[$pos]->getContent();
        }
        return md5($fingerprintBase);
    }

    /** Check constraints */
    public function completeCloneClass(): bool
    {
        $constraintsSatisfied = $this->constraints->allSatisfied($this->currentCloneClass);
        if ($constraintsSatisfied) {
            $this->results->add($this->currentCloneClass);
        }
        return $constraintsSatisfied;
    }

    /** Return list containing all retained cloneObject classes
     * @var CloneClass[] */
    public function getCloneClasses(): array
    {
        return $this->results->getCloneClasses();
    }
}

class GapDetectingCloneConsumer extends CloneConsumer
{

    /** The first clone of the clone class.
        * @var CloneObject */
    private $firstClone = null;

    /** The position of the first clone. */
    private $firstPos = 0;

    /** The length of the first clone. */
    private $firstLength = 0;

    /** Constructor. */
    public function __construct(array $word)
    {
        parent::__construct($word);
    }

    public function startCloneClass(int $normalizedLength): void
    {
        parent::startCloneClass($normalizedLength);
        $this->firstClone = null;
    }

    public function addClone(int $globalPosition, int $length): CloneObject
    {
        // get clone without gap information
        /** @var CloneObject */
        $clone = parent::addClone($globalPosition, $length);

        $delta = Delta::computeDeltaStatic(
            array_slice($this->units, $this->firstPos, $this->firstPos + $this->firstLength),
            array_slice($this->units, $globalPosition, $globalPosition + $length)
        );

        if ($this->firstClone !== null) {
            $clone->setDeltaInUnits($delta->getSize());
            $element = $this->resolveElement($clone->getUniformPath());
            $this->fillGaps($clone, $delta, $globalPosition, $element);
        } else {
            $clone->setDeltaInUnits(0);
            $this->firstClone = $clone;
            $this->firstPos = $globalPosition;
            $this->firstLength = $length;
        }

        return $clone;
    }

    /**
     * @param Delta<Unit> $delta
     */
    private function fillGaps(CloneObject $clone, Delta $delta, int $globalPosition, TextRegionLocation $element): void
    {
        $firstNeedsGaps = !$this->firstClone->containsGaps();

        for ($i = 0; $i < $delta->getSize(); ++$i) {
            $pos = $delta->getPosition($i);
            if ($pos > 0) {
                $pos--;
                /** @var Unit */
                $unit = $this->units[$globalPosition + $pos];
                $rawStartOffset = $element->getUnfilteredOffset($unit->getFilteredStartOffset());
                $rawEndOffset = $element->getUnfilteredOffset($unit->getFilteredEndOffset());
                $clone->addGap(new Region($rawStartOffset, $rawEndOffset));
            } else if ($firstNeedsGaps) {
                $pos = -$pos - 1;
                $unit = $this->units[$this->firstPos + $pos];
                $rawStartOffset = $element->getUnfilteredOffset($unit->getFilteredStartOffset());
                $rawEndOffset = $element->getUnfilteredOffset($unit->getFilteredEndOffset());
                $this->firstClone->addGap(new Region($rawStartOffset, $rawEndOffset));
            }
        }
    }
}
